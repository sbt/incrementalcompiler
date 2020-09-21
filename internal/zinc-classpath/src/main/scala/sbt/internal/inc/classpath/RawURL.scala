/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package sbt
package internal
package inc
package classpath

import java.io.{ ByteArrayInputStream, InputStream }
import java.net.{ Proxy, URL, URLConnection, URLStreamHandler }
import java.util.Enumeration

object RawURL {

  /**
   * Constructs a URL with scheme `raw` and path `file` that will return the bytes for `value` in the platform default encoding
   * when a connection to the URL is opened.
   */
  def apply(file: String, value: String): URL = apply(file, value.getBytes)

  /** Constructs a URL with scheme `raw` and path `file` that will return the bytes `value` when a connection to the URL is opened. */
  def apply(file: String, value: Array[Byte]): URL = apply(file)(new ByteArrayInputStream(value))

  /**
   * Constructs a URL with scheme `raw` and path `file` that will use `value` to construct the `InputStream` used when a connection
   * to the URL is opened.
   */
  def apply(file: String)(value: => InputStream): URL =
    new URL("raw", null, -1, file, new RawStreamHandler(value))

  private[this] final class RawStreamHandler(value: => InputStream) extends URLStreamHandler {
    override protected def openConnection(url: URL, p: Proxy): URLConnection = openConnection(url)

    override protected def openConnection(url: URL): URLConnection = new URLConnection(url) {
      private lazy val in = value
      def connect(): Unit = { in; () }
      override def getInputStream = in
    }

  }

}

/** A ClassLoader that looks up resource requests in a `Map` prior to the base ClassLoader's resource lookups. */
trait RawResources extends FixedResources {

  /** The map from resource paths to the raw String content to provide via the URL returned by [[sbt.internal.inc.classpath.FixedResources!.findResource(s:String)*]] or [[findResources]]. */
  protected def resources: Map[String, String]
  override protected final val resourceURL = resources.transform(RawURL.apply)
}

/** A ClassLoader that looks up resource requests in a `Map` prior to the base ClassLoader's resource lookups. */
trait FixedResources extends ClassLoader {

  /** The map from resource paths to URL to provide in [[sbt.internal.inc.classpath.FixedResources!.findResource(s:String)*]] and [[findResources]]. */
  protected def resourceURL: Map[String, URL]

  override def findResource(s: String): URL = resourceURL.getOrElse(s, super.findResource(s))

  import java.util.Collections.{ enumeration, singletonList }

  override def findResources(s: String): Enumeration[URL] = {
    val sup = super.findResources(s)
    resourceURL.get(s) match {
      case Some(url) => new DualEnumeration(enumeration(singletonList(url)), sup)
      case None      => sup
    }
  }

}
