package tfd.coderover.ui

import _root_.tfd.coderover._
import java.awt.{BorderLayout, Color, Dimension, GridBagLayout, GridBagConstraints, Font, Rectangle}
import java.awt.event.{ActionEvent, ItemListener, ItemEvent}

import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter}

import javax.swing.{AbstractAction, Action, DefaultComboBoxModel, ImageIcon, JButton, JComboBox, JLabel, JOptionPane, JPanel, JProgressBar, JFileChooser, JFrame, JMenuBar, JMenu, JMenuItem, JScrollPane, JSplitPane, JTextPane, JToolBar}
import javax.swing.filechooser.{FileNameExtensionFilter}
import javax.swing.text.{StyleConstants, SimpleAttributeSet, AttributeSet, DefaultStyledDocument}

import _root_.tfd.gui.swing.CutCopyPastePopupSupport
import _root_.tfd.gui.swing.codesyntaxpane.CodeSyntaxDocument
import _root_.tfd.scala.properties.{HasBindableProperties}

import tasks.{DeserializeTaskManager, TaskManager, Scenario}
import xml.XML

class MainApplication() extends HasBindableProperties {
  import ThreadControl._

  private var currentFile:File = _

  private val taskManagerProperty = BindableProperty[TaskManager]("taskManager", null)

  private val currentScenarioProperty = BindableProperty[Scenario]("currentScenario", null)

  private var currentEnvironment:GUIEnvironment = new GUIEnvironment(10, 10, Set.empty, Set.empty, None, Map.empty, Map.empty)

  private var viewController:GUIViewController = new GUIViewController(50, State(2,2,0), currentEnvironment, None, DefaultConstraints)

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

  private lazy val loadAction = new AbstractAction("Load") {
    override def actionPerformed(ae:ActionEvent) {
      loadFile("XML TaskSet Files", "xml") { file =>
          viewController.stop
          fork {
              taskManagerProperty := DeserializeTaskManager(XML.loadFile(file))
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

  private lazy val openAction = new AbstractAction("Open") {
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
  
  private lazy val saveAction = new AbstractAction("Save") {
    import javax.swing.JOptionPane._

    override def actionPerformed(ae:ActionEvent) {
      val chooser = new JFileChooser
      chooser.setFileFilter(new FileNameExtensionFilter("CORAL Files", "coral"))
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
            writer.close
        }
      }
    }
  }

  private lazy val runAction:AbstractAction = new AbstractAction("Run") {

    override def actionPerformed(ae:ActionEvent) {
      new Thread(new RunWorker()).start
    }
  }

  private lazy val runTaskAction:AbstractAction = new AbstractAction("Run Task") {
    override def actionPerformed(ae:ActionEvent) {
      progressBar.setValue(0)
      progressBar.setMinimum(0)
      progressBar.setMaximum(taskManagerProperty.get.currentTask.scenarios.length)
      progressBar.setForeground(Color.GREEN)
      new Thread(new RunTaskWorker()).start
    }
  }

  abstract private class BaseRunWorker extends Runnable {

    protected class TaskCompletionStatus(evaluationResult:ResultOrAbend[Any]) {
      val stopped = viewController.stopped
      val runningTask = (taskManagerProperty.get != null)
      val complete = if (runningTask) currentScenarioProperty.get.isComplete(currentEnvironment, viewController.currentState) else false
      private[this] val sb = new StringBuffer()
      if (runningTask) {
        sb.append(currentScenarioProperty.get)
        sb.append(" - ")
      }
      sb.append(
        if (stopped) {
          "Stopped by user"
        } else {
          if (runningTask) {
            if (evaluationResult.abend == None) {
              if (complete) {
                "SUCCESS !!"
              } else {
                "FAILED !!"
              }
            } else {
              evaluationResult.abend.get.message
            }
          } else {
            "" 
          }
        }
      )
      val message = sb.toString
      val failed = (stopped || (runningTask && !complete))
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
           runTaskAction.setEnabled(taskManagerProperty.get != null)
           scenarioCombo.setEnabled(true)
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
        while (scenarioIndex < taskManagerProperty.get.currentTask.scenarios.length && !failed) {
          onEDTWait {
            scenarioCombo.setSelectedIndex(scenarioIndex)
          }
          val taskCompletionStatus = runCurrentTask(parseResult)
          failed = taskCompletionStatus.failed
          scenarioIndex += 1
          onEDTLater {
            progressBar.setValue(scenarioIndex)
            if (taskCompletionStatus.failed) {
              progressBar.setForeground(Color.RED)
            }
          }
        }
        if (!failed) {
           taskManagerProperty.get.nextTask()
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

    setMnemonic('F')

    def menuItem(action:Action, mnemonic:Char) = {
      val mi = new JMenuItem(action)
      mi.setMnemonic(mnemonic)
      mi
    }
    add(menuItem(loadAction, 'L'))
    add(menuItem(newAction, 'N'))
    add(menuItem(openAction, 'O'))
    add(menuItem(saveAction, 'S'))
  }
  menuBar.add(menu)
  frame.setJMenuBar(menuBar)

  private val progressBar = new JProgressBar
  private val taskLabel = new JLabel()
  private val scenarioCombo = new JComboBox()

  currentScenarioProperty.onChange { scenario =>
    viewController = scenario.createController()
    currentEnvironment = viewController.environment
    viewController.printDelegate = printDelegate
    viewController.syncToStartState()
    //viewController.syncEnvironment()
    gridPane.setViewportView(viewController.getView);
  }

  taskManagerProperty.onChange { taskManager =>
    updateTaskAndScenarios       
  }

  def updateTaskAndScenarios() {
    taskLabel.setText(taskManagerProperty.get.currentTask.toString)
    currentScenarioProperty.set(taskManagerProperty.get.currentTask.scenarios(0))
    scenarioCombo.setModel(new DefaultComboBoxModel(taskManagerProperty.get.currentTask.scenarios.toArray.asInstanceOf[Array[Object]]))
    scenarioCombo.addItemListener(new ItemListener() {
      override def itemStateChanged(ie:ItemEvent) {
        if (ie.getStateChange == ItemEvent.SELECTED) {
           currentScenarioProperty.set(ie.getItem().asInstanceOf[Scenario])
        }
      }
    })
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
    val panel = new JPanel
    panel.setOpaque(false)
    panel.setLayout(new GridBagLayout())
    val gbc = new GridBagConstraints();
    import GridBagConstraints._
    gbc.anchor = NORTHWEST
    gbc.fill = BOTH
    gbc.weightx = 1.0
    panel.add(progressBar, gbc)
    progressBar.setLayout(new BorderLayout())
    progressBar.add(new JLabel("Task : "), BorderLayout.WEST)
    progressBar.add(taskLabel)
    gbc.fill = VERTICAL
    gbc.weightx = 0.0
    panel.add(createButtonForAction(runTaskAction, "go-go.png", "Run all scenarios"), gbc)
    panel.add(scenarioCombo, gbc)
    add(panel)
    add(createButtonForAction(runAction, "go.png", "Run for current scenario"))
    add(createButtonForAction(stopAction, "stop.png", "Stop running program"))
  }

  private val gridPane = new JScrollPane()

  private[this] val codeCanvasPane = new JSplitPane(
    JSplitPane.HORIZONTAL_SPLIT,
    new JScrollPane(codeText),
    gridPane
    )
  val codeTextDoc = new CodeSyntaxDocument(codeText)
  codeText.setDocument(codeTextDoc)

  codeCanvasPane.setDividerLocation(265)
  codeCanvasPane.setPreferredSize(new Dimension(780, 505))
  contentPane.add(toolBar, BorderLayout.NORTH)

  private[this] val consolePane = new JScrollPane(consoleText)
  consolePane.setPreferredSize(new Dimension(700,120))

  contentPane.add(new JSplitPane(
    JSplitPane.VERTICAL_SPLIT,
    codeCanvasPane,
    consolePane
    ),BorderLayout.CENTER)

  runTaskAction.setEnabled(false)
  viewController.printDelegate = printDelegate
  viewController.syncToStartState()
  //viewController.syncEnvironment()
  gridPane.setViewportView(viewController.getView);

  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.pack
  frame.setVisible(true)
}