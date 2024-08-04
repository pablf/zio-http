/*
 * Copyright 2023 the ZIO HTTP contributors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package zio.http

import zio._

import zio.http.codec.PathCodec

/*
 * Represents a single route, which has either handled its errors by converting
 * them into responses, or which has polymorphic errors, which must later be
 * converted into responses before the route can be executed.
 *
 * Routes have the property that, before conversion into handlers, they will
 * fully handle all errors, including defects, translating them appropriately
 * into responses that can be delivered to clients. Thus, the handlers returned
 * by `toHandler` will never fail, and will always produce a valid response.
 *
 * Individual routes can be aggregated using [[zio.http.Routes]].
 */
sealed trait Route[-Env, +Err] { self =>
  import Route.{Augmented, Handled, Provided, Unhandled}

  /**
   * Includes error details in the body of the response.
   */
  def includeErrorDetails: Route[Env, Err] =
    self match {
      case Provided(route, env)     => Provided(route.includeErrorDetails, env)
      case Augmented(route, aspect) => Augmented(route.includeErrorDetails, aspect)
      case r @ Handled(_, _, _)     => r.errorDetails(true)
      case r @ Unhandled(_, _, _)   => r.errorDetails(true)
    }

  /**
   * Excludes error details from the body of the response.
   */
  def excludeErrorDetails: Route[Env, Err] =
    self match {
      case Provided(route, env)     => Provided(route.excludeErrorDetails, env)
      case Augmented(route, aspect) => Augmented(route.excludeErrorDetails, aspect)
      case r @ Handled(_, _, _)     => r.errorDetails(false)
      case r @ Unhandled(_, _, _)   => r.errorDetails(false)
    }

  /**
   * Applies the route to the specified request. The route must be defined for
   * the request, or else this method will fail fatally. Note that you may only
   * call this function when you have handled all errors produced by the route,
   * converting them into responses.
   */
  final def apply(request: Request)(implicit ev: Err <:< Response, trace: Trace): ZIO[Env, Response, Response] =
    toHandler.apply(request)

  def asErrorType[Err2](implicit ev: Err <:< Err2): Route[Env, Err2] = self.asInstanceOf[Route[Env, Err2]]

  /**
   * Handles all typed errors in the route by converting them into responses.
   * This method can be used to convert a route that does not handle its errors
   * into one that does handle its errors.
   */
  final def handleError(f: Err => Response)(implicit trace: Trace): Route[Env, Nothing] =
    self.handleErrorCause(Response.fromCauseWith(_)(f))

  final def handleErrorZIO(f: Err => ZIO[Any, Nothing, Response])(implicit trace: Trace): Route[Env, Nothing] =
    self.handleErrorCauseZIO { cause =>
      cause.failureOrCause match {
        case Left(err)    => f(err)
        case Right(cause) => ZIO.succeed(Response.fromCause(cause))
      }
    }

  /**
   * Handles all typed errors, as well as all non-recoverable errors, by
   * converting them into responses. This method can be used to convert a route
   * that does not handle its errors into one that does handle its errors.
   */
  final def handleErrorCause(f: Cause[Err] => Response)(implicit trace: Trace): Route[Env, Nothing] =
    self match {
      case Provided(route, env)                     => Provided(route.handleErrorCause(f), env)
      case Augmented(route, aspect)                 => Augmented(route.handleErrorCause(f), aspect)
      case Handled(routePattern, handler, location) =>
        Handled(
          routePattern,
          err => handler(err).map(_.mapErrorCause(c => f(c.asInstanceOf[Cause[Nothing]]))),
          location,
        )

      case Unhandled(rpm, handler, zippable, location) =>
        val handler2: Boolean => Handler[Any, Nothing, RoutePattern[_], Handler[Env, Response, Request, Response]] = {
          errorInBody =>
            Handler.fromFunction[RoutePattern[_]] { pattern =>
              val paramHandler = {
                Handler.fromFunctionZIO[(rpm.Context, Request)] { case (ctx, request) =>
                  pattern.asInstanceOf[RoutePattern[rpm.PathInput]].decode(request.method, request.path) match {
                    case Left(error)  => ZIO.dieMessage(error)
                    case Right(value) =>
                      val params = rpm.zippable.zip(value, ctx)

                      handler(zippable.zip(params, request))
                  }
                }
              }

              // Sandbox before applying aspect:
              rpm.aspect.applyHandlerContext(paramHandler.mapErrorCause(f))
            }
        }

        Handled(rpm.routePattern, handler2, location)
    }

  /**
   * Handles all typed errors, as well as all non-recoverable errors, by
   * converting them into a ZIO effect that produces the response. This method
   * can be used to convert a route that does not handle its errors into one
   * that does handle its errors.
   */
  final def handleErrorCauseZIO(
    f: Cause[Err] => ZIO[Any, Nothing, Response],
  )(implicit trace: Trace): Route[Env, Nothing] =
    self match {
      case Provided(route, env)                     => Provided(route.handleErrorCauseZIO(f), env)
      case Augmented(route, aspect)                 => Augmented(route.handleErrorCauseZIO(f), aspect)
      case Handled(routePattern, handler, location) =>
        Handled[Env](
          routePattern,
          err => handler(err).map(_.mapErrorCauseZIO(c => f(c.asInstanceOf[Cause[Nothing]]))),
          location,
        )

      case Unhandled(rpm, handler, zippable, location) =>
        val handler2: Boolean => Handler[Any, Nothing, RoutePattern[_], Handler[Env, Response, Request, Response]] = {
          errorInBody =>
            Handler.fromFunction[RoutePattern[_]] { pattern =>
              val paramHandler =
                Handler.fromFunctionZIO[(rpm.Context, Request)] { case (ctx, request) =>
                  pattern.asInstanceOf[RoutePattern[rpm.PathInput]].decode(request.method, request.path) match {
                    case Left(error)  => ZIO.dieMessage(error)
                    case Right(value) =>
                      val params = rpm.zippable.zip(value, ctx)

                      handler(zippable.zip(params, request))
                  }
                }
              rpm.aspect.applyHandlerContext(paramHandler.mapErrorCauseZIO(f))
            }
        }

        Handled(rpm.routePattern, handler2, location)
    }

  /**
   * Allows the transformation of the Err type through a function allowing one
   * to build up a Routes in Stages targets the Unhandled case
   */
  final def mapError[Err1](fxn: Err => Err1)(implicit trace: Trace): Route[Env, Err1] = {
    self match {
      case Provided(route, env)                        => Provided(route.mapError(fxn), env)
      case Augmented(route, aspect)                    => Augmented(route.mapError(fxn), aspect)
      case Handled(routePattern, handler, location)    => Handled[Env](routePattern, handler, location)
      case Unhandled(rpm, handler, zippable, location) => Unhandled(rpm, handler.mapError(fxn), zippable, location)
    }

  }

  /**
   * Allows the transformation of the Err type through an Effectful program
   * allowing one to build up a Routes in Stages targets the Unhandled case
   * only.
   */
  final def mapErrorZIO[Err1](fxn: Err => ZIO[Any, Err1, Response])(implicit trace: Trace): Route[Env, Err1] =
    self match {
      case Provided(route, env)                        => Provided(route.mapErrorZIO(fxn), env)
      case Augmented(route, aspect)                    => Augmented(route.mapErrorZIO(fxn), aspect)
      case Handled(routePattern, handler, location)    => Handled[Env](routePattern, handler, location)
      case Unhandled(rpm, handler, zippable, location) => Unhandled(rpm, handler.mapErrorZIO(fxn), zippable, location)
    }

  /**
   * Handles all typed errors in the route by converting them into responses,
   * taking into account the request that caused the error. This method can be
   * used to convert a route that does not handle its errors into one that does
   * handle its errors.
   */
  final def handleErrorRequest(f: (Err, Request) => Response)(implicit trace: Trace): Route[Env, Nothing] =
    self.handleErrorRequestCause((request, cause) => Response.fromCauseWith(cause)(f(_, request)))

  /**
   * Handles all typed errors, as well as all non-recoverable errors, by
   * converting them into responses, taking into account the request that caused
   * the error. This method can be used to convert a route that does not handle
   * its errors into one that does handle its errors.
   */
  final def handleErrorRequestCause(f: (Request, Cause[Err]) => Response)(implicit trace: Trace): Route[Env, Nothing] =
    self match {
      case Provided(route, env)                     => Provided(route.handleErrorRequestCause(f), env)
      case Augmented(route, aspect)                 => Augmented(route.handleErrorRequestCause(f), aspect)
      case Handled(routePattern, handler, location) =>
        Handled[Env](
          routePattern,
          err =>
            handler(err).map { handler =>
              Handler.fromFunctionHandler[Request] { (req: Request) =>
                handler.mapErrorCause(c => f(req, c.asInstanceOf[Cause[Nothing]]))
              }
            },
          location,
        )

      case Unhandled(rpm, handler, zippable, location) =>
        val handler2: Boolean => Handler[Any, Nothing, RoutePattern[_], Handler[Env, Response, Request, Response]] = {
          errorInBody =>
            Handler.fromFunction[RoutePattern[_]] { pattern =>
              val paramHandler =
                Handler.fromFunctionZIO[(rpm.Context, Request)] { case (ctx, request) =>
                  pattern.asInstanceOf[RoutePattern[rpm.PathInput]].decode(request.method, request.path) match {
                    case Left(error)  => ZIO.dieMessage(error)
                    case Right(value) =>
                      val params = rpm.zippable.zip(value, ctx)

                      handler(zippable.zip(params, request))
                  }
                }

              // Sandbox before applying aspect:
              rpm.aspect.applyHandlerContext(
                Handler.fromFunctionHandler[(rpm.Context, Request)] { case (_, req) =>
                  paramHandler.mapErrorCause(f(req, _))
                },
              )
            }
        }

        Handled(rpm.routePattern, handler2, location)
    }

  /**
   * Handles all typed errors, as well as all non-recoverable errors, by
   * converting them into a ZIO effect that produces the response, taking into
   * account the request that caused the error. This method can be used to
   * convert a route that does not handle its errors into one that does handle
   * its errors.
   */
  final def handleErrorRequestCauseZIO(
    f: (Request, Cause[Err]) => ZIO[Any, Nothing, Response],
  )(implicit trace: Trace): Route[Env, Nothing] =
    self match {
      case Provided(route, env)                     => Provided(route.handleErrorRequestCauseZIO(f), env)
      case Augmented(route, aspect)                 => Augmented(route.handleErrorRequestCauseZIO(f), aspect)
      case Handled(routePattern, handler, location) =>
        Handled[Env](
          routePattern,
          err =>
            handler(err).map { handler =>
              Handler.fromFunctionHandler[Request] { (req: Request) =>
                handler.mapErrorCauseZIO(c => f(req, c.asInstanceOf[Cause[Nothing]]))
              }
            },
          location,
        )

      case Unhandled(rpm, handler, zippable, location) =>
        val handler2: Boolean => Handler[Any, Nothing, RoutePattern[_], Handler[Env, Response, Request, Response]] = {
          errorInBody =>
            Handler.fromFunction[RoutePattern[_]] { pattern =>
              val paramHandler =
                Handler.fromFunctionZIO[(rpm.Context, Request)] { case (ctx, request) =>
                  pattern.asInstanceOf[RoutePattern[rpm.PathInput]].decode(request.method, request.path) match {
                    case Left(error)  => ZIO.dieMessage(error)
                    case Right(value) =>
                      val params = rpm.zippable.zip(value, ctx)

                      handler(zippable.zip(params, request))
                  }
                }
              rpm.aspect.applyHandlerContext(
                Handler.fromFunctionHandler[(rpm.Context, Request)] { case (_, req) =>
                  paramHandler.mapErrorCauseZIO(f(req, _))
                },
              )
            }
        }

        Handled(rpm.routePattern, handler2, location)
    }

  /**
   * Determines if the route is defined for the specified request.
   */
  final def isDefinedAt(request: Request): Boolean = routePattern.matches(request.method, request.path)

  /**
   * The location where the route was created, which is useful for debugging
   * purposes.
   */
  def location: Trace

  def nest(prefix: PathCodec[Unit]): Route[Env, Err] =
    self match {
      case Provided(route, env)                     => Provided(route.nest(prefix), env)
      case Augmented(route, aspect)                 => Augmented(route.nest(prefix), aspect)
      case Handled(routePattern, handler, location) => Handled[Env](routePattern.nest(prefix), handler, location)

      case Unhandled(rpm, handler, zippable, location) =>
        Unhandled(rpm.prefix(prefix), handler, zippable, location)
    }

  final def provideEnvironment(env: ZEnvironment[Env]): Route[Any, Err] =
    Route.Provided(self, env)

  /**
   * The route pattern over which the route is defined. The route can only
   * handle requests that match this route pattern.
   */
  def routePattern: RoutePattern[_]

  /**
   * Applies the route to the specified request. The route must be defined for
   * the request, or else this method will fail fatally.
   */
  final def run(request: Request)(implicit trace: Trace): ZIO[Env, Either[Err, Response], Response] =
    Routes(self).run(request)

  /**
   * Returns a route that automatically translates all failures into responses,
   * using best-effort heuristics to determine the appropriate HTTP status code,
   * and attaching error details using the HTTP header `Warning`.
   */
  final def sandbox(implicit trace: Trace): Route[Env, Nothing] =
    handleErrorCause(Response.fromCause(_))

  def toHandler(implicit ev: Err <:< Response, trace: Trace): Handler[Env, Response, Request, Response]

  final def toRoutes: Routes[Env, Err] = Routes(self)

  def transform[Env1](
    f: Handler[Env, Response, Request, Response] => Handler[Env1, Response, Request, Response],
  ): Route[Env1, Err] =
    Route.Augmented(self, f)
}
object Route                   {

  def handled[Env](
    routePattern: RoutePattern[_],
  )(handler: Handler[Env, Response, Request, Response])(implicit trace: Trace): Route[Env, Nothing] = {
    // Sandbox before constructing:
    Route.Handled(
      routePattern,
      errorInBody => Handler.fromFunction[RoutePattern[_]](_ => handler.sandbox(errorInBody)),
      Trace.empty,
    )
  }

  def handled[Params, Env](rpm: Route.Builder[Env, Params]): HandledConstructor[Env, Params] =
    new HandledConstructor[Env, Params](rpm)

  val notFound: Route[Any, Nothing] =
    Handled(RoutePattern.any, _ => Handler.fromFunction[RoutePattern[_]](_ => Handler.notFound), Trace.empty)

  def route[Params](routePattern: RoutePattern[Params]): UnhandledConstructor[Any, Params] =
    route(Route.Builder(routePattern, HandlerAspect.identity))

  def route[Params, Env](rpm: Route.Builder[Env, Params]): UnhandledConstructor[Env, Params] =
    new UnhandledConstructor[Env, Params](rpm)

  final class HandledConstructor[-Env, Params](val rpm: Route.Builder[Env, Params]) extends AnyVal {
    def apply[Env1 <: Env, In](
      handler: Handler[Env1, Response, In, Response],
      errorInBody: Boolean,
    )(implicit zippable: Zippable.Out[Params, Request, In], trace: Trace): Route[Env1, Nothing] = {
      val handler2: Boolean => Handler[Any, Nothing, RoutePattern[_], Handler[Env1, Response, Request, Response]] = {
        err =>
          Handler.fromFunction[RoutePattern[_]] { pattern =>
            val paramHandler =
              Handler.fromFunctionZIO[(rpm.Context, Request)] { case (ctx, request) =>
                pattern.asInstanceOf[RoutePattern[rpm.PathInput]].decode(request.method, request.path) match {
                  case Left(error)  => ZIO.dieMessage(error)
                  case Right(value) =>
                    val params = rpm.zippable.zip(value, ctx)

                    handler(zippable.zip(params, request))
                }
              }

            // Sandbox before applying aspect:
            rpm.aspect.applyHandlerContext(paramHandler.sandbox(err))
          }
      }

      Handled(rpm.routePattern, handler2, trace).errorDetails(errorInBody)
    }
  }

  final class UnhandledConstructor[-Env, Params](val rpm: Route.Builder[Env, Params]) extends AnyVal {
    def apply[Env1 <: Env, Err, Input](
      handler: Handler[Env1, Err, Input, Response],
    )(implicit zippable: Zippable.Out[Params, Request, Input], trace: Trace): Route[Env1, Err] =
      Unhandled(rpm, handler, zippable, trace)
  }

  /**
   * A combination of a route pattern and aspect, used for building routes that
   * depend on aspect context (such as authentication).
   */
  sealed abstract class Builder[-Env, A] { self =>
    type PathInput
    type Context

    def routePattern: RoutePattern[PathInput]
    def aspect: HandlerAspect[Env, Context]
    def zippable: Zippable.Out[PathInput, Context, A]

    /**
     * Constructs a route from this route pattern and aspect.
     */
    def ->[Env1 <: Env, Err, I](handler: Handler[Env1, Err, I, Response])(implicit
      input: RequestHandlerInput[A, I],
      trace: Trace,
    ): Route[Env1, Err] = {
      implicit val z = input.zippable

      Route.route[A, Env1](self)(handler)
    }

    def prefix(path: PathCodec[Unit]): Builder[Env, A] =
      new Builder[Env, A] {
        type PathInput = self.PathInput
        type Context   = self.Context

        def routePattern: RoutePattern[PathInput]         = self.routePattern.nest(path)
        def aspect: HandlerAspect[Env, Context]           = self.aspect
        def zippable: Zippable.Out[PathInput, Context, A] = self.zippable
      }

    def provideEnvironment(env: ZEnvironment[Env]): Route.Builder[Any, A] = {
      implicit val z = zippable

      Route.Builder(routePattern, aspect.provideEnvironment(env))
    }
  }
  object Builder                         {
    def apply[Env, PI, MC, Out](rp: RoutePattern[PI], mc: HandlerAspect[Env, MC])(implicit
      z: Zippable.Out[PI, MC, Out],
    ): Route.Builder[Env, Out] =
      new Route.Builder[Env, Out] {
        type PathInput = PI
        type Context   = MC

        def routePattern: RoutePattern[PathInput]           = rp
        def aspect: HandlerAspect[Env, Context]             = mc
        def zippable: Zippable.Out[PathInput, Context, Out] = z
      }
  }

  private final case class Provided[Env, +Err](
    route: Route[Env, Err],
    env: ZEnvironment[Env],
  ) extends Route[Any, Err] {
    def location: Trace = route.location

    def routePattern: RoutePattern[_] = route.routePattern

    override def toHandler(implicit ev: Err <:< Response, trace: Trace): Handler[Any, Response, Request, Response] =
      route.toHandler.provideEnvironment(env)

    override def toString() = s"Route.Provided(${route}, ${env})"
  }

  private final case class Augmented[InEnv, -OutEnv, +Err](
    route: Route[InEnv, Err],
    aspect: Handler[InEnv, Response, Request, Response] => Handler[OutEnv, Response, Request, Response],
  ) extends Route[OutEnv, Err] {
    def location: Trace = route.location

    def routePattern: RoutePattern[_] = route.routePattern

    override def toHandler(implicit ev: Err <:< Response, trace: Trace): Handler[OutEnv, Response, Request, Response] =
      aspect(route.toHandler)

    override def toString() = s"Route.Augmented(${route}, ${aspect})"
  }

  private final case class Handled[-Env](
    routePattern: RoutePattern[_],
    handler: Boolean => Handler[Any, Nothing, RoutePattern[_], Handler[Env, Response, Request, Response]],
    location: Trace,
  ) extends Route[Env, Nothing] {

    private var errorInBody = false

    private[http] def errorDetails(v: Boolean): Route[Env, Nothing] = {
      errorInBody = v
      self
    }

    override def toHandler(implicit ev: Nothing <:< Response, trace: Trace): Handler[Env, Response, Request, Response] =
      Handler.fromZIO(handler(errorInBody)(routePattern)).flatten

    override def toString() = s"Route.Handled(${routePattern}, ${location})"
  }
  private final case class Unhandled[Params, Input, -Env, +Err](
    rpm: Route.Builder[Env, Params],
    handler: Handler[Env, Err, Input, Response],
    zippable: Zippable.Out[Params, Request, Input],
    location: Trace,
  ) extends Route[Env, Err] { self =>

    private var errorInBody = false

    private[http] def errorDetails(v: Boolean): Route[Env, Nothing] = {
      errorInBody = v
      self
    }

    def routePattern = rpm.routePattern

    override def toHandler(implicit ev: Err <:< Response, trace: Trace): Handler[Env, Response, Request, Response] = {
      convert(handler.asErrorType[Response])
    }

    override def toString() = s"Route.Unhandled(${routePattern}, ${location})"

    private def convert[Env1 <: Env](
      handler: Handler[Env1, Response, Input, Response],
    )(implicit trace: Trace): Handler[Env1, Response, Request, Response] = {
      implicit val z = zippable

      Route.handled(rpm)(handler, errorInBody).toHandler
    }
  }

}
