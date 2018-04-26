package com.igeolise.geoliser.utils

import java.io.Closeable
import java.nio.file.Files

import org.apache.commons.io.FileUtils


class TempDirResource(prefix: String) extends Closeable {
  val tempDir = Files.createTempDirectory(prefix)
  override def close(): Unit = FileUtils.deleteDirectory(tempDir.toFile)
}