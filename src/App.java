import javafx.application.Application;
import javafx.application.Platform;
import javafx.beans.binding.DoubleBinding;
import javafx.scene.Group;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.shape.Line;
import javafx.scene.text.Text;
import javafx.stage.Stage;

public class App extends Application {
    public static void main(String args[]) {
        launch();
    }

    public void start(Stage primaryStage){
        Text t = new Text("x");
        HBox h = new HBox(t);
        Group g = new Group(h);

        Line l = new Line();
        DoubleBinding innerWidth = javafx.beans.binding.Bindings.createDoubleBinding(
                () -> g.layoutBoundsProperty().get().getWidth(),
                g.layoutBoundsProperty());
        l.endXProperty().bind(innerWidth);

        VBox v = new VBox(l, g);
        v.boundsInLocalProperty().addListener((a,b,c) -> {});

        Group g2 = new Group(v);
        g2.layoutBoundsProperty().addListener((a,b,c) -> {});

        h.getChildren().remove(t);

        Platform.exit();
    }
}
