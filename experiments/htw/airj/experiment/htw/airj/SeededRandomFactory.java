package experiment.htw.airj;

import java.util.Random;

public final class SeededRandomFactory {
  private SeededRandomFactory() {
  }

  public static Random fromString(String text) {
    try {
      return new Random(Long.parseLong(text));
    } catch (NumberFormatException ignored) {
      return new Random();
    }
  }
}
