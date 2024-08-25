import scala.swing.*
import scala.swing.event.*

object TicTacToe extends SimpleSwingApplication:
  // This is the frame inside the GUI window
  // `top` is the GUI equivalent of the main method
  def top: Frame = new MainFrame:
    // The title of the frame
    title = "Tic Tac Toe"

    val buttons: Array[Array[Button]] = Array.ofDim[Button](3, 3)
    // We refer to a player as simply a string corresponding to their marker
    var currentPlayer = "X"
    var gameOver = false

    val statusLabel = new Label(s"Current player: $currentPlayer")
    val restartButton = new Button("Restart Game")
    restartButton.visible = false // The restart button will be visible only after the game ends

    // This is a grid with 3 rows and 3 columns
    // We create a button for each cell in the grid.
    // Each of the button becomes a child component of the 3 x 3 grid
    val gridPanel: GridPanel = new GridPanel(3, 3):
      for
        i <- 0 until 3
        j <- 0 until 3
      do
        // Create a new button
        val button = new Button("") {
          preferredSize = new Dimension(100, 100)
          font = new Font("Arial", java.awt.Font.BOLD, 40)
          name = s"button_$i$j"
        }

        buttons(i)(j) = button // Add this button to the buttons 2D array
        contents += button // Add this button as a child to the grid

    // This BorderPanel is the content of the main panel
    // It consists of the previous gridPanel and a FlowPanel at the bottom (south)
    contents = new BorderPanel:
      layout(gridPanel) = BorderPanel.Position.Center
      layout(new FlowPanel(statusLabel, restartButton)) = BorderPanel.Position.South

    // Register the restart button and each of the grid cell buttons as sources of events that the main panel will handle
    listenTo(restartButton)
    buttons.flatten.foreach(listenTo(_))

    // Handle the events that are incoming from the buttons we have registered to listen to
    reactions += {
      // Handle the button click event from the button we can refer to as source
      case ButtonClicked(source) =>
        println(s"Button clicked: ${source.name}")
        // Conditionally handle the button click event depending on whether it came from the restart button or one
        // of the grid cell buttons
        source match
          // A button was clicked
          // Check if the game was not over, the clicked button was, indeed, one of the grid cell buttons, and the
          // text is empty (meaning that it has not been marked)
          case button: Button if !gameOver && buttons.flatten.contains(button) && button.text.isEmpty =>
            println(s"Valid move on button: ${button.name}")
            button.text = currentPlayer // Mark the grid cell
            if checkWin() then // If a player has won
              gameOver = true
              statusLabel.text = s"Player $currentPlayer wins!"
              restartButton.visible = true
            else if isBoardFull then // If the game is tied
              gameOver = true
              statusLabel.text = "It's a draw!"
              restartButton.visible = true
            else // The next player can have a go
              currentPlayer = if currentPlayer == "X" then "O" else "X"
              statusLabel.text = s"Current player: $currentPlayer"
          case `restartButton` =>
            println("Restart button clicked")
            resetGame()
          case _ =>
            println("Unhandled button click")
    }

    def checkWin(): Boolean =
      // Get all the possible lines
      val lines =
        (0 until 3).map(i => buttons(i).toList) ++ // rows
          (0 until 3).map(i => buttons.map(_(i)).toList) ++ // columns
          List(
            List(buttons(0)(0), buttons(1)(1), buttons(2)(2)), // diagonal
            List(buttons(0)(2), buttons(1)(1), buttons(2)(0)) // other diagonal
          )
      // Check if there exists a line such that all the marked texts are the same
      // Also check that the distinct marker is not an empty string (meaning that the line has not been marked, at all)
      lines.exists(line => line.map(_.text).distinct.size == 1 && line.head.text.nonEmpty)

    // The board is full if all the buttons have been marked
    def isBoardFull: Boolean =
      buttons.flatten.forall(_.text.nonEmpty)

    // Whenever the properties of a component change, the component is repainted
    // In this game, we essentially change the properties of the components to that of the beginning of the game,
    // and swing takes care of repainting them
    def resetGame(): Unit =
      for
        i <- 0 until 3
        j <- 0 until 3
      do
        buttons(i)(j).text = ""
      currentPlayer = "X"
      gameOver = false
      statusLabel.text = s"Current player: $currentPlayer"
      restartButton.visible = false
