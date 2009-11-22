package tfd.coderover.ui

import java.awt.EventQueue

class Worker extends Thread {
  
  def onEDTWait(block: => Unit) = {
	  EventQueue.invokeAndWait(new Thread() {
	    override def run() = block
	  })
  }

  def onEDTLater(block: => Unit) = {
	  EventQueue.invokeLater(new Thread() {
	    override def run() = block
	  })
  }
}
