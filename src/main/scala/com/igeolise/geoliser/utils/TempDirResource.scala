package com.igeolise.geoliser.utils

import java.io.Closeable
import java.nio.file.Files


class TempDirResource(prefix: String) extends Closeable {
  val tempDir = Files.createTempDirectory(prefix)
  override def close(): Unit = Files.delete(tempDir)
}