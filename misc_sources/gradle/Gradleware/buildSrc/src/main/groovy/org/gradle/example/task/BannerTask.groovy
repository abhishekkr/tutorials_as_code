/**/

import org.gradle.api.DefaultTask
import org.gradle.api.tasks.TaskAction

class BannerTask extends DefaultTask {
  def msg

  @TaskAction
  def printBanner(){
    println "~" * 50
    println msg
    println "~" * 50
  }
}
