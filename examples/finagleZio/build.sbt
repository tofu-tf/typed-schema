PB.targets in Compile := Seq(
  scalapb.gen(javaConversions = false, flatPackage = false)
    -> (sourceManaged in Compile).value
)
PB.protoSources := Seq(sourceDirectory.value / "main" / "protobuf")
PB.deleteTargetDirectory := false
