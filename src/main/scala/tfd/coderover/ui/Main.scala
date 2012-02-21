package tfd.coderover.ui

import javax.swing.plaf.synth.SynthLookAndFeel
import javax.swing.UIManager

object Main {
	def main(args:Array[String]) {
     System.setProperty("awt.useSystemAAFontSettings", "lcd");
     //val  synth = new SynthLookAndFeel();
     //synth.load(classOf[Main].getResourceAsStream("demo.xml"), Main.class);
     //UIManager.setLookAndFeel(synth);
     new MainApplication()
  	}
}