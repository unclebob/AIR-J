package experiment.htw.airj;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.Set;

public final class RotatingDoubleTorus {
  private static final int WIDTH = 10;
  private static final int CELL_COUNT = 100;
  private static final Set<String> PITS = Set.of("O12", "O27", "O44", "O67", "O81",
      "I18", "I33", "I58", "I72", "I96");
  private static final Set<String> BATS = Set.of("O5", "O20", "O46", "O70", "O88",
      "I9", "I24", "I31", "I63", "I94");

  private RotatingDoubleTorus() {
  }

  public static String allRooms() {
    var rooms = new ArrayList<String>(CELL_COUNT * 2);
    for (int i = 0; i < CELL_COUNT; i++) {
      rooms.add("O" + i);
    }
    for (int i = 0; i < CELL_COUNT; i++) {
      rooms.add("I" + i);
    }
    return String.join(" ", rooms);
  }

  public static boolean validRoom(String room) {
    return parseRoom(room) != null;
  }

  public static String neighbors(String room, int rotation) {
    var parsed = parseRoom(room);
    if (parsed == null) {
      throw new IllegalArgumentException("Invalid room: " + room);
    }
    int row = parsed.index / WIDTH;
    int col = parsed.index % WIDTH;
    return String.join(" ",
        room(parsed.torus, cell(row, col + 1)),
        room(parsed.torus, cell(row + 1, col)),
        room(parsed.torus, cell(row, col - 1)),
        room(parsed.torus, cell(row - 1, col)),
        bridgeRoom(room, rotation));
  }

  public static String neighborText(String room, int rotation) {
    return "Tunnels lead to " + neighbors(room, rotation).replace(" ", ", ");
  }

  public static boolean isPit(String room) {
    return PITS.contains(room);
  }

  public static boolean isBat(String room) {
    return BATS.contains(room);
  }

  public static String randomRoom(Random rng) {
    return room(rng.nextBoolean() ? 'O' : 'I', rng.nextInt(CELL_COUNT));
  }

  public static String randomNeighbor(Random rng, String room, int rotation) {
    var choices = neighbors(room, rotation).split(" ");
    return choices[rng.nextInt(choices.length)];
  }

  public static String moveWumpus(Random rng, String room, int rotation) {
    int roll = rng.nextInt(6);
    if (roll == 0) {
      return room;
    }
    var choices = neighbors(room, rotation).split(" ");
    return choices[roll - 1];
  }

  private static String bridgeRoom(String room, int rotation) {
    var parsed = parseRoom(room);
    if (parsed.torus == 'O') {
      return room('I', Math.floorMod(parsed.index + rotation, CELL_COUNT));
    }
    return room('O', Math.floorMod(parsed.index - rotation, CELL_COUNT));
  }

  private static int cell(int row, int col) {
    return Math.floorMod(row, WIDTH) * WIDTH + Math.floorMod(col, WIDTH);
  }

  private static String room(char torus, int index) {
    return torus + Integer.toString(index);
  }

  private static ParsedRoom parseRoom(String room) {
    if (room == null || room.length() < 2) {
      return null;
    }
    char torus = room.charAt(0);
    if (torus != 'O' && torus != 'I') {
      return null;
    }
    try {
      int index = Integer.parseInt(room.substring(1));
      return index >= 0 && index < CELL_COUNT ? new ParsedRoom(torus, index) : null;
    } catch (NumberFormatException ignored) {
      return null;
    }
  }

  private record ParsedRoom(char torus, int index) {
  }
}
