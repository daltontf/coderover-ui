package tfd.coderover.ui

import _root_.tfd.coderover.ui.tasks._
import javax.swing.{UIManager}

import com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel

object Main {
	def main(args:Array[String]) {
     //UIManager.setLookAndFeel(new NimbusLookAndFeel)
		 new MainApplication()
  	}
}