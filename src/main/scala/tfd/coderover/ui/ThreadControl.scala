package tfd.coderover.ui

import java.awt.EventQueue

object ThreadControl {
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

  def fork(block: => Unit) = {
      val thread = new Thread(new Runnable() {
        override def run() {
          block
        }
      }) 
      thread.start
      thread
  }
}

