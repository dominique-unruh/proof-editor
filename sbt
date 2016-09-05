set -e
SBT_URL=https://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/0.13.9/sbt-launch.jar
test -e tmp/sbt-launch.jar || wget "$SBT_URL" -O tmp/sbt-launch.jar
export JAVA_HOME=/usr/java/latest # Should be Oracle Java
"$JAVA_HOME/bin/java" -jar tmp/sbt-launch.jar "$@"
