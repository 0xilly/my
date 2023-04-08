load("@rules_java//java:defs.bzl", "java_binary", "java_library")

java_library(
    name = "libpsalm",
    srcs = glob(
      ["src/main/java/io/ansan/psalm/**/*.java"],
      exclude = ["src/main/java/io/ansan/psalm/driver/*"],
    ),
)
