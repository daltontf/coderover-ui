package tfd.coderover.ui

import _root_.tfd.coderover._
import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter}

import javax.swing.filechooser.{FileNameExtensionFilter}
import _root_.tfd.gui.swing.CutCopyPastePopupSupport
import _root_.tfd.gui.swing.codesyntaxpane.CodeSyntaxDocument

import tasks.{DeserializeTaskManager, TaskManager, Scenario}
import xml.XML
import javax.swing.text._
import java.awt.event._
import java.awt.{List =>_, _}
import javax.swing._
import border.EmptyBorder
import tfd.scala.events.EventSource

class MainApplication() {
  import ThreadControl._

  private var currentFile:File = _

  private var taskManager:Option[TaskManager] = None

  private var currentScenario:Option[Scenario] = None

  private var currentEnvironment:GUIEnvironment = new GUIEnvironment(8, 10, Set.empty, Set.empty, None, Map.empty, Map.empty)

  private var viewController:GUIViewController = new GUIViewController(45, State(2,2,0), currentEnvironment, None, DefaultConstraints)

  private var evaluator:Evaluator = new Evaluator()

  private val languageParser = new LanguageParser()

  private val codeFont = new Font("Courier", Font.BOLD, 12)

  private val printDelegate = Some((value:String) =>
      onEDTLater {
        consoleTextAppend(value, null)
      }
    )

  private val codeText = new JTextPane();
  { 	import codeText._
    setToolTipText("Enter code here")
    setFont(codeFont)
    CutCopyPastePopupSupport.enable(codeText)
  }

  lazy private val blueTextAttribute = {
    val attributeSet = new SimpleAttributeSet
    StyleConstants.setForeground(attributeSet, Color.BLUE.darker)
    attributeSet
  }

  lazy private val redTextAttribute = {
    val attributeSet = new SimpleAttributeSet
    StyleConstants.setForeground(attributeSet, Color.RED)
    attributeSet
  }

  private val consoleText = new JTextPane();
  {   import consoleText._
    setEditable(false)
    setDocument(new DefaultStyledDocument())
    setToolTipText("Console messages")
    setFont(codeFont)
    CutCopyPastePopupSupport.enable(consoleText)
  }

  private def consoleTextAppend(text:String, attributeSet:AttributeSet)() {
    var endPos = consoleText.getStyledDocument.getLength
    if (endPos > 0) {
      consoleText.getStyledDocument.insertString(endPos, "\n", attributeSet)
      endPos = endPos + 1
    }
    consoleText.getStyledDocument.insertString(endPos, text, attributeSet)
  }

  private def icon(s:String) = new ImageIcon(getClass().getClassLoader().getResource(s))

  private def loadFile(description:String, extension:String)(block:File => Unit) {
    val chooser = new JFileChooser
    chooser.setFileFilter(new FileNameExtensionFilter(description, extension))
    if (chooser.showOpenDialog(frame) == JFileChooser.APPROVE_OPTION) {
       val file = chooser.getSelectedFile
       if (file.exists) {
          block(file)
       }
    }
  }

  val newTaskManager = new EventSource[TaskManager]
  val newCurrentScenario = new EventSource[Scenario]

  newTaskManager.foreach { tm =>
    taskManager = Some(tm)
    runTaskAction.setEnabled(true)
    scenarioCombo.setEnabled(true)
    displayHelpAction.setEnabled(true)
    updateTaskAndScenarios
  }

  newCurrentScenario.foreach { scenario =>
    currentScenario = Some(scenario)
    viewController = scenario.createController()
    currentEnvironment = viewController.environment
    viewController.printDelegate = printDelegate
    viewController.syncToStartState()
    gridPane.setViewportView(viewController.getView)
  }

  private lazy val openTaskSetAction = new AbstractAction("Task Set") {
    override def actionPerformed(ae:ActionEvent) {
      loadFile("XML Task Set Files", "xml") { file =>
          viewController.stop
          fork {
              newTaskManager.fire(DeserializeTaskManager(XML.loadFile(file)))
          }
      }
    }
  }

  private lazy val newAction = new AbstractAction("New") {
    override def actionPerformed(ae:ActionEvent) {
      viewController.stop
      codeText.setText("")
    }
  }

  private lazy val openCoralAction = new AbstractAction("CORAL") {
    override def actionPerformed(ae:ActionEvent) {
      loadFile("CORAL Files", "coral") { file =>
        viewController.stop
        fork {
          val reader = new BufferedReader(new FileReader(file))
          val buffer = new StringBuffer(file.length.asInstanceOf[Int])
          while (reader.ready) {
            buffer.append(reader.readLine)
            buffer.append("\n")
          }
          reader.close
          onEDTLater {
            codeText.setText(buffer.toString)
          }
        }
      }
    }
  }
  
  private lazy val saveCoralAction = new AbstractAction("CORAL") {
    import javax.swing.JOptionPane._

    private lazy val chooser = {
      val it = new JFileChooser
      it.setFileFilter(new FileNameExtensionFilter("CORAL Files", "coral"))
      it
    }

    override def actionPerformed(ae:ActionEvent) {
       if (chooser.showSaveDialog(frame) == JFileChooser.APPROVE_OPTION) {
        val selectedFile = chooser.getSelectedFile
        val file = if (selectedFile.getName.indexOf(".") >= 0) {
          chooser.getSelectedFile
        } else {
          new File(selectedFile.getAbsolutePath + ".coral")
        }
        if (file.exists() &&
                JOptionPane.showConfirmDialog(frame, "File Exists", "Existing file will be overwritten", OK_CANCEL_OPTION) != OK_OPTION) {
          return
        }
        fork {
            val writer = new BufferedWriter(new FileWriter(file))
            val text = codeText.getText
            writer.write(text, 0, text.length)
            writer.close()
        }
      }
    }
  }

  private lazy val runAction:AbstractAction = new AbstractAction("Run") {

    override def actionPerformed(ae:ActionEvent) {
      new Thread(new RunWorker()).start()
    }
  }

  private lazy val runTaskAction:AbstractAction = new AbstractAction("Run Task") {
    override def actionPerformed(ae:ActionEvent) {
      progressBar.setValue(0)
      progressBar.setMinimum(0)
      progressBar.setMaximum(taskManager.get.currentTask.scenarios.length)
      progressBar.setForeground(Color.GREEN)
      new Thread(new RunTaskWorker()).start
    }
  }

  private lazy val displayHelpAction:AbstractAction = new AbstractAction("Help") {
    override def actionPerformed(ae:ActionEvent) {
      glassPane.setVisible(true);
    }
  }

  abstract private class BaseRunWorker extends Runnable {

    protected class TaskCompletionStatus(evaluationResult:ResultOrAbend[Any]) {
      val stopped = viewController.stopped
      val runningTask = (taskManager != None)
      val complete = if (runningTask) currentScenario.get.isComplete(currentEnvironment, viewController.currentState) else false
      val message = {
        val sb = new StringBuilder()
        if (runningTask) {
          sb.append(currentScenario.get)
          sb.append(" - ")
        }
        sb.append(
          if (stopped) {
            "Stopped by user"
          } else {
            if (evaluationResult.abend == None) {
              if (runningTask) {
                if (complete) {
                  "SUCCESS !!"
                } else {
                  "FAILED !!"
                }
              } else {
                ""
              }
            } else {
              evaluationResult.abend.get.message
            }
          }
          )
        sb
      }.toString
      val failed = (stopped || evaluationResult.abend != None || (runningTask && !complete))
    }

    def doRun()

    def consoleTextAppendOnEDT(text:String, attributeSet:AttributeSet) {
      onEDTLater(consoleTextAppend(text, attributeSet))
    }

    def consoleTextAppendOnEDT(taskCompletionStatus:TaskCompletionStatus) {
      consoleTextAppendOnEDT(taskCompletionStatus.message, if (taskCompletionStatus.failed) redTextAttribute else blueTextAttribute)
    }

    def runCurrentTask(parseResult:languageParser.ParseResult[List[Instruction]]) = {
      viewController.syncToStartState()
      viewController.syncEnvironment()
      val taskCompletionStatus = new TaskCompletionStatus(evaluator.evaluate(parseResult.get, viewController))
      consoleTextAppendOnEDT(taskCompletionStatus)
      taskCompletionStatus
    }

    override def run() {
      onEDTWait {
          consoleText.setText("")
          consoleText.scrollRectToVisible(new Rectangle(0,0,1,1))
          runAction.setEnabled(false)
          runTaskAction.setEnabled(false)
          scenarioCombo.setEnabled(false)
          stopAction.setEnabled(true)
      }
      try {
        doRun()
      } finally {
         viewController.repaint()
         onEDTLater {
           runAction.setEnabled(true)
           runTaskAction.setEnabled(taskManager != None)
           scenarioCombo.setEnabled(taskManager != None)
           stopAction.setEnabled(false)
         }
      }
    }
  }

  private class RunTaskWorker extends BaseRunWorker {

    override def doRun() {
      viewController.reset()
      val parseResult = languageParser.parse(codeText.getText.toUpperCase)
      consoleTextAppendOnEDT(parseResult.toString, blueTextAttribute)
      if (parseResult.successful) {
        var scenarioIndex = 0
        var failed = false
        while (scenarioIndex < taskManager.get.currentTask.scenarios.length && !failed) {
          onEDTWait {
            scenarioCombo.setSelectedIndex(scenarioIndex)
          }
          val taskCompletionStatus = runCurrentTask(parseResult)
          failed = taskCompletionStatus.failed
          scenarioIndex += 1
          onEDTLater {
            progressBar.setValue(scenarioIndex)
            if (failed) {
              progressBar.setForeground(Color.RED)
            }
          }
        }
        if (!failed) {
           taskManager.get.nextTask()
           updateTaskAndScenarios()
        }
      }
    }
  }

  private class RunWorker extends BaseRunWorker {

    override def doRun() {
      viewController.reset()
      val parseResult = languageParser.parse(codeText.getText.toUpperCase)
      consoleTextAppendOnEDT(parseResult.toString, blueTextAttribute)
      if (parseResult.successful) {
        runCurrentTask(parseResult)
      }
    }
  }

  private lazy val stopAction = new AbstractAction("Stop") {
    setEnabled(false)

    override def actionPerformed(ae:ActionEvent) {
      viewController.stop
    }
  }

  private val frame = new JFrame("Code Rover")
  private val menuBar = new JMenuBar;
  private val menu = new JMenu("File"); {
    import menu._

    setMnemonic(KeyEvent.VK_F)

    def menuItem(action:Action, mnemonic:Char) = {
      val mi = new JMenuItem(action)
      mi.setMnemonic(mnemonic)
      mi
    }
    add(menuItem(newAction, 'N'))
    val openSubMenu = new JMenu("Open")
    openSubMenu.setMnemonic('O')
    add(openSubMenu)
    openSubMenu.add(menuItem(openTaskSetAction, 'T'))
    openSubMenu.add(menuItem(openCoralAction, 'C'))
    val saveSubMenu = new JMenu("Save")
    saveSubMenu.setMnemonic('S')
    add(saveSubMenu)
    saveSubMenu.add(menuItem(saveCoralAction, 'C'))

    add(menuItem(new AbstractAction("Exit") {
      def actionPerformed(e: ActionEvent) = {
        System.exit(0);
      }
    }, 'x'))
  }
  menuBar.add(menu)
  frame.setJMenuBar(menuBar)

  private val progressBar = new JProgressBar
  private val taskLabel = new JLabel("<html><i>Not Loaded</i></html>")
  private val scenarioCombo = new JComboBox()

  def updateTaskAndScenarios() {
    taskManager.foreach {tm =>
      taskLabel.setText(tm.currentTask.toString)
      newCurrentScenario.fire(tm.currentTask.scenarios(0))
      scenarioCombo.setModel(new DefaultComboBoxModel(tm.currentTask.scenarios.toArray.asInstanceOf[Array[Object]]))
      scenarioCombo.addItemListener(new ItemListener() {
        override def itemStateChanged(ie:ItemEvent) {
          if (ie.getStateChange == ItemEvent.SELECTED) {
            newCurrentScenario.fire(ie.getItem().asInstanceOf[Scenario])
          }
        }
      })
      instructionLabel.setText("<html><center><h2>" + tm.currentTask.title + "</h2><br/>&nbsp;</br>" + tm.currentTask.description + "</center></html>")
      glassPane.setVisible(true);
    }
  }

  private val contentPane = frame.getContentPane

  private val toolBar = new JToolBar; {
    import toolBar._

    setFloatable(false)

    def createButtonForAction(action:AbstractAction, iconFile:String, toolTipText:String) = {
      val btn = new JButton(action); { import btn._
        setText("")
        setIcon(icon(iconFile))
        setToolTipText(toolTipText)
      }
      btn
    }
    setOpaque(false)
    setLayout(new GridBagLayout())
    val gbc = new GridBagConstraints()
    import GridBagConstraints._
    gbc.anchor = NORTHWEST
    gbc.fill = BOTH
    gbc.weightx = 1.0
    add(progressBar, gbc)
    progressBar.setLayout(new BorderLayout())
    progressBar.add(new JLabel("Task : "), BorderLayout.WEST)
    progressBar.add(taskLabel)
    gbc.fill = VERTICAL
    gbc.weightx = 0.0
    add(scenarioCombo, gbc)
    add(createButtonForAction(displayHelpAction, "help.png", "Display task instructions"), gbc)
    add(createButtonForAction(runTaskAction, "go-go.png", "Run all scenarios"), gbc)
    add(createButtonForAction(runAction, "go.png", "Run for current scenario"), gbc)
    add(createButtonForAction(stopAction, "stop.png", "Stop running program"), gbc)
  }

  private val gridPane = new JScrollPane()

  private def setTabs(textPane:JTextPane, charactersPerTab:Int) {
		val tabWidth = textPane.getFontMetrics(textPane.getFont).charWidth('w') * charactersPerTab

		val tabs = new Array[TabStop](10)

		for (j <- 0 until tabs.length) {
			val tab = j + 1
			tabs(j) = new TabStop( tab * tabWidth )
		}

		val tabSet = new TabSet(tabs)
		val attributes = new SimpleAttributeSet()
		StyleConstants.setTabSet(attributes, tabSet)
		val length = textPane.getDocument.getLength
		textPane.getStyledDocument.setParagraphAttributes(0, length, attributes, false)
	}

  val codeScrollPane = new JScrollPane(codeText)
  private[this] val codeCanvasPane = new JSplitPane(
    JSplitPane.HORIZONTAL_SPLIT,
    codeScrollPane,
    gridPane
    )
  val codeTextDoc = new CodeSyntaxDocument(codeText)
  codeText.setDocument(codeTextDoc)
  setTabs(codeText, 4)
  codeScrollPane.setRowHeaderView( new TextLineNumber(codeText, 1) );

  codeCanvasPane.setDividerLocation(390)
  codeCanvasPane.setPreferredSize(new Dimension(780, 505))
  contentPane.add(toolBar, BorderLayout.NORTH)

  private[this] val consolePane = new JScrollPane(consoleText)
  consolePane.setPreferredSize(new Dimension(700,120))

  val verticalSplit: JSplitPane = new JSplitPane(
    JSplitPane.VERTICAL_SPLIT,
    codeCanvasPane,
    consolePane
  )
  verticalSplit.setDividerLocation(470)
  contentPane.add(verticalSplit,BorderLayout.CENTER)

  runTaskAction.setEnabled(false)
  scenarioCombo.setEnabled(false)
  displayHelpAction.setEnabled(false)
  viewController.printDelegate = printDelegate
  viewController.syncToStartState()
  gridPane.setViewportView(viewController.getView)

  val glassPane = new GlassPane
  glassPane.setLayout(new BorderLayout)
  val instructionLabel = new JLabel
  instructionLabel.setHorizontalAlignment(SwingConstants.CENTER)
  glassPane.add(instructionLabel, BorderLayout.CENTER)
  glassPane.add({
    val pnl = new JPanel
    pnl.setOpaque(false)
    pnl.add({
      val btn = new JButton("OK")
      btn.addActionListener(new ActionListener() {
        def actionPerformed(e: ActionEvent) = {
          glassPane.setVisible(false)
        }
      })
      btn
    })
    pnl
  }, BorderLayout.SOUTH)
  frame.setGlassPane(glassPane)
  glassPane.setBorder(BorderFactory.createCompoundBorder(new EmptyBorder(30,30,30,30), BorderFactory.createLineBorder(Color.BLACK, 1)))
  glassPane.setVisible(false)

  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.pack
  frame.setVisible(true)
}