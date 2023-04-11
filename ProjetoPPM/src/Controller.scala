import javafx.fxml.FXML
import javafx.scene.{Scene, SubScene}
import javafx.scene.control.{Button, Label, TextField}
import javafx.scene.layout.HBox
import javafx.stage.Stage


class Controller {

  @FXML
  private var subScene1:SubScene = _
  @FXML
  private var labelScale: Label = _
  @FXML
  private var labelScale2: Label = _
  @FXML
  private var labelGreenRemove: Label = _
  @FXML
  private var labelSepiaEffect: Label = _
  @FXML
  private var labelWelcome: Label = _
  @FXML
  private var buttonScale2: Button = _
  @FXML
  private var buttonScale05: Button = _
  @FXML
  private var buttonSepiaEffect: Button = _
  @FXML
  private var buttonGreenRemove: Button = _


  var number = 1
  //method automatically invoked after the @FXML fields have been injected
  /*
  @FXML
  def initialize(): Unit = {
    InitSubScene.subScene.widthProperty.bind(subScene1.widthProperty)
    InitSubScene.subScene.heightProperty.bind(subScene1.heightProperty)
    subScene1.setRoot(InitSubScene.root)
  }

   */

  def onButtonScale05Clicked(): Unit = {
    labelWelcome.setText("changes: " + number)
    number = number + 1
    //OctreeUtils.scale(Octree,0.5)
    }

  def onButtonScale2Clicked(): Unit = {
    labelWelcome.setText("changes: " + number )
    number = number + 1
    //OctreeUtils.scale(Octree,2)

  }
  def onButtonGreenRemoveClicked() {
    //do smth
    labelWelcome.setText("changes: " + number )
    number = number + 1
    //OctreeUtils.greenRemove()

}

  def onButtonSerpiaEffectClicked(): Unit = {


    //do smth
    labelWelcome.setText("changes: " + number )
    number = number + 1

    OctreeUtils.
  }
}