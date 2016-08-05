package tryouts;

/**
 * Created by unruh on 8/3/16.
 */
public class Y {
    static {
        System.out.println("Loaded");
    }
    private Y() {
        System.out.println("Constructed");
    }
    static void x() {}
}
