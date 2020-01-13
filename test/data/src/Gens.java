import java.util.List;
import java.util.ArrayList;

public class Gens <TVar extends List<TVar>> extends ArrayList<TVar> {
  public TVar f;
  public TVar m(TVar x) {
    return null;
  };

  class InternalGens <X extends TVar> {
    public TVar g;
    public <G extends X, F extends Z, Z> TVar n(TVar x, X t, G g) {
      return null;
    };

    class SuperInternalGens <Z extends TVar> {
      public TVar g;
      public <U extends Z> TVar n(Z a) {
        return null;
      };
    }
  }
}
