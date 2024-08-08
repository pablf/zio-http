package zio.http.security

import zio._
import zio.test._

import zio.schema._

import zio.http._
import zio.http.codec._
import zio.http.endpoint._
import java.nio.file
import java.io.File

object StaticFileServingSpec extends ZIOSpecDefault {
    // tests Middleware.serveResources, Middleware.serveDirectory, Middleware.cors

  // Helper function for creating a mock directory structure for testing
  def mkDir(root: String) = ZIO.attempt {
    val root = Files.createDirectory(Paths.get("").toAbsolutePath(), root)

    val secrets       = Files.createDirectory(root.resolve("secrets"))
    val secret       = Files.createFile(secrets.resolve("secret"))

    val dir       = Files.createDirectory(tempDir.resolve("/local/dir"))
    val file = Files.createFile(dir.resolve("file"))
    Files.write(file, "something".getBytes())
    Files.createSymbolicLink(dir.resolve("symlink"), symlinkContent)

    
    val resources = Files.createDirectory("/src/main/resources")
    val content = Files.createFile(resources.resolve("content"))
    Files.write(content, "something".getBytes())
    Files.createSymbolicLink(resources.resolve("symlink"), symlinkContent)

    val symlinks  = Files.createDirectory(dir2.resolve("symlinks"))
    val symlinkContent = Files.createFile(dir4.resolve("symlinkContent"))
    Files.write(symlinkContent, "something".getBytes())

  }.unit
    val root:file.Path = ???

  // Helper function for deleting a mock directory structure for testing
  val deleteDir = ZIO.attempt(Files.deleteAll(tempDir))

  // Initializing static server (Hosting files from the mock directory structure)
  val path: String           = "/local/dir"

  val serveDirectory = (Method.GET / "serveDirectory" -> Handler.ok).toHttpApp @@ Middleware.serveDirectory(Path("dirs"), new File(path))
  val serveResources = (Method.GET / "serveResources" -> Handler.ok).toHttpApp @@ Middleware.serveResources(Path.empty)
  val cors           = (Method.GET / "cors" -> Handler.ok).toHttpApp @@ Middleware.cors(file.Path("cors"), new File(path))

  val serveDirectoryPaths = Gen.fromIterable(
    List(
        URL(Root / "serveDirectory" / "dirs" / ".." / "dir"),
        URL(Root / "serveDirectory" / "dirs" / "file" / ".." / "file"),
        URL(Root / "serveDirectory" / "dirs" / ".." / ".." / "local" / "dir"),
        URL(Root / "serveDirectory" / "dirs" / ".." / ".." / ".." / ".." / "secrets" / "secret"),
        URL(Root / "serveDirectory" / "dirs" / ".." / ".." / ".." / ".." / "secrets"),
        URL(Root / "serveDirectory" / "dirs" / "....//....//etc//passwd"),
        //windows
        URL(Root / "serveDirectory" / "dirs" / "..\\..\\etc\\passwd"),
        //symlink
        URL(Root / "serveDirectory" / "dirs" / "symlink" / "symlinkContent"),
    )
  )

  val paths2 = Gen.fromIterable(
    List(
        //encoded
        URL(Root / "files" / "%2e%2e%2f%2e%2e%2fetc%2fpasswd"),
        URL(Root / "files" / "%uff0e%uff0e%u2215%uff0e%uff0e%u2215etc%u2215passwd"),
        URL(Root / "files" / "%252e%252e%252f%252e%252e%252fetc%252fpasswd"),
    )
  )

  val serveResourcesPaths = Gen.fromIterable(
    List(
        URL(Root / "serveResources" / ".." / ".." / "secrets"),
        URL(Root / "serveResources" / ".." / ".." / "secrets" / "secret"),
        URL(Root / "serveResources" / "content" / ".." / ".." / ".." / "secrets"),
        URL(Root / "serveResources" / "content" / ".." / ".." / ".." / "secrets" / secret),
        URL(Root / "serveResources" / "symlink" / ".." / ".." / "local" / "dir"),
        URL(Root / "serveResources" / "symlink" / ".." / ".." / "local" / "dir"),
        //windows
        URL(Root / "serveResources" / "dirs" / "..\\..\\etc\\passwd"),
        //symlink
        URL(Root / "serveResources" / "dirs" / "symlink" / "symlinkContent"),
    )
  )

  def spec =
    suite("StaticFileServingSpec")(
      suite("Middleware.serveDirectory")(
        test("DotDotSlash") {
            check(serveDirectoryPaths) {path =>
                for {
                    response <- serveDirectory.runZIO(Request(path))
                    status = r.status
                    body = r.body
                } yield assertTrue(status == Status.BadRequest) && assertZIO(body.asString)(equalTo(""))
            }
        },
        test("Encoded") {
            check(paths2) {path =>
                for {
                    response <- serveDirectory.runZIO(Request(path))
                    status = r.status
                    body = r.body
                } yield assertTrue(status == Status.NotFound) && assertZIO(body.asString)(equalTo(""))
            }
        },
      ),
    suite("Middleware.serveResources")(
        test("DotDotSlash") {
            check(serveResourcesPaths) {path =>
                for {
                    response <- serveResources.runZIO(Request(path))
                    status = r.status
                    body = r.body
                } yield assertTrue(status == Status.BadRequest) && assertZIO(body.asString)(equalTo(""))
            }
        },
        test("Encoded") {
            check(paths2) {path =>
                for {
                    response <- serveResources.runZIO(Request(path))
                    status = r.status
                    body = r.body
                } yield assertTrue(status == Status.NotFound) && assertZIO(body.asString)(equalTo(""))
            }
        },
      ),
    ) @@ TestAspect.beforeAll(mkDir(root)) @@ TestAspect.afterAll(deleteDir(root))
}
