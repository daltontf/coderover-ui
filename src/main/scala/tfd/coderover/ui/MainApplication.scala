package tfd.coderover.ui

import java.awt.{BorderLayout, Color, Dimension, GridBagLayout, GridBagConstraints, Font, Rectangle}
import java.awt.event.{ActionEvent, ItemListener, ItemEvent}

import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter}

import javax.swing.{AbstractAction, Action, DefaultComboBoxModel, ImageIcon, JButton, JComboBox, JLabel, JOptionPane, JPanel, JProgressBar, JFileChooser, JFrame, JMenuBar, JMenu, JMenuItem, JScrollPane, JSplitPane, JTextPane, JToolBar}
import javax.swing.filechooser.{FileNameExtensionFilter}
import javax.swing.text.{StyleConstants, SimpleAttributeSet, AttributeSet, DefaultStyledDocument}

import _root_.tfd.gui.swing.CutCopyPastePopupSupport
import _root_.tfd.gui.swing.codesyntaxpane.CodeSyntaxDocument
import _root_.tfd.scala.properties.{HasBindableProperties}

import _root_.tfd.coderover.{Instruction, State, LanguageParser, Evaluator}

import tasks.{TaskManager, Scenario}

class MainApplication(taskManager:TaskManager) extends HasBindableProperties {
  import edt._

  private var currentFile:File = _

  private val currentScenario = BindableProperty[Scenario]("currentScenario", null)
  
  private var currentState:State = _

  private var currentEnvironment:GUIEnvironment = _

  private var viewController:GUIViewController = _

  private var evaluator:Evaluator = _

  private val languageParser = new LanguageParser()

  private val codeFont = new Font("Courier", Font.BOLD, 12)

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

  private lazy val newAction = new AbstractAction("New") {
    override def actionPerformed(ae:ActionEvent) {
      currentState.stopped = true
      codeText.setText("")
    }
  }

  private lazy val openAction = new AbstractAction("Open") {
    override def actionPerformed(ae:ActionEvent) {
      val chooser = new JFileChooser
      chooser.setFileFilter(new FileNameExtensionFilter("Code Rover Files", "coderover"))
      if (chooser.showOpenDialog(frame) == JFileChooser.APPROVE_OPTION) {
        val file = chooser.getSelectedFile
        if (file.exists) {
          currentState.stopped = true
          new Thread(new Runnable() {
            override def run() = {
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
          }).start
        }
      }
    }
  }

  private lazy val saveAction = new AbstractAction("Save") {
    import javax.swing.JOptionPane._

    override def actionPerformed(ae:ActionEvent) {
      val chooser = new JFileChooser
      chooser.setFileFilter(new FileNameExtensionFilter("Code Rover Files", "coderover"))
      if (chooser.showSaveDialog(frame) == JFileChooser.APPROVE_OPTION) {
        val selectedFile = chooser.getSelectedFile
        val file = if (selectedFile.getName.indexOf(".") >= 0) {
          chooser.getSelectedFile
        } else {
          new File(selectedFile.getAbsolutePath + ".coderover")
        }
        if (file.exists() &&
                JOptionPane.showConfirmDialog(frame, "File Exists", "Existing file will be overwritten", OK_CANCEL_OPTION) != OK_OPTION) {
          return
        }
        new Thread(new Runnable() {
          override def run() {
            val writer = new BufferedWriter(new FileWriter(file))
            val text = codeText.getText
            writer.write(text, 0, text.length)
            writer.close
          }
        }).start
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
      progressBar.setMaximum(taskManager.currentTask.scenarios.length)
      progressBar.setForeground(Color.GREEN)
      new Thread(new RunTaskWorker()).start
    }
  }

  abstract private class BaseRunWorker extends Runnable {

    protected class TaskCompletionStatus() {
      val stopped = currentState.stopped
      val complete = taskManager.currentTask.isComplete(currentEnvironment, currentState)
      private[this] val sb = new StringBuffer()
      sb.append(currentScenario.get)
      sb.append(" - ")
      sb.append(
        if (stopped) {
          currentState.abend match {
            case Some(abend) => abend.message
            case None => "Stopped by user"
          }
        } else {
          if (complete) {
            "SUCCESS !!"
          } else {
            "FAILED !!"
          }
        }
        )
      val message = sb.toString

      val failed = (stopped || !complete)
    }

    def doRun()

    def consoleTextAppendOnEDT(text:String, attributeSet:AttributeSet) {
      onEDTLater(consoleTextAppend(text, attributeSet))
    }

    def consoleTextAppendOnEDT(taskCompletionStatus:TaskCompletionStatus) {
      consoleTextAppendOnEDT(taskCompletionStatus.message, if (taskCompletionStatus.failed) redTextAttribute else blueTextAttribute)
    }

    def runCurrentTask(parseResult:languageParser.ParseResult[List[Instruction]]) = {
      currentState = currentScenario.get.createStartState
      viewController.syncToState(currentState)
      viewController.syncEnvironment()
      evaluator.evaluate(parseResult.get, currentState)
      val taskCompletionStatus = new TaskCompletionStatus()
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
         viewController.canvas.repaint()
         onEDTLater {
           runAction.setEnabled(true)
           runTaskAction.setEnabled(true)
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
        while (scenarioIndex < taskManager.currentTask.scenarios.length && !failed) {
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
           taskManager.nextTask()
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
      currentState.stopped = true
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

    add(menuItem(newAction, 'N'))
    add(menuItem(openAction, 'O'))
    add(menuItem(saveAction, 'S'))
  }
  menuBar.add(menu)
  frame.setJMenuBar(menuBar)

  private val progressBar = new JProgressBar
  private val taskLabel = new JLabel()
  private val scenarioCombo = new JComboBox()

  currentScenario.onChange { scenario =>
    currentEnvironment = scenario.environmentFactory()
    viewController = new GUIViewController(50, currentEnvironment) {
      override def print(value:String) = onEDTLater {
        consoleTextAppend(value, null)
      }
    }
    currentState = scenario.createStartState()
    viewController.syncToState(currentState)
    viewController.syncEnvironment()
    gridPane.setViewportView(viewController.canvas);
    evaluator = new Evaluator(currentEnvironment, viewController)
  }

  def updateTaskAndScenarios() {
    taskLabel.setText(taskManager.currentTask.toString)
    currentScenario.set(taskManager.currentTask.scenarios(0))
    scenarioCombo.setModel(new DefaultComboBoxModel(taskManager.currentTask.scenarios.toArray.asInstanceOf[Array[Object]]))
    scenarioCombo.addItemListener(new ItemListener() {
      override def itemStateChanged(ie:ItemEvent) {
        if (ie.getStateChange == ItemEvent.SELECTED) {
           currentScenario.set(ie.getItem().asInstanceOf[Scenario])
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

  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.pack

  updateTaskAndScenarios()

  frame.setVisible(true)
}