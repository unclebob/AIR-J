import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.Scanner;
import java.util.Set;

public final class HuntTheWumpus {
  private static final int WIDTH = 10;
  private static final int CELL_COUNT = 100;
  private static final Set<String> PITS = Set.of("O12", "O27", "O44", "O67", "O81",
      "I18", "I33", "I58", "I72", "I96");
  private static final Set<String> BATS = Set.of("O5", "O20", "O46", "O70", "O88",
      "I9", "I24", "I31", "I63", "I94");
  private static final String START_ROOM = "O0";
  private static final String START_WUMPUS = "O55";
  private static final int START_ARROWS = 5;
  private static final String COMMAND_HELP =
      "Commands: move <room>, shoot <r1> <r2> ... <r5>, status, help, quit";

  private record GameState(String playerRoom, String wumpusRoom, int arrows, int rotation) {
  }

  public static void main(String[] args) {
    var game = new HuntTheWumpus(makeRandom(args));
    game.play();
  }

  private final Random random;

  private HuntTheWumpus(Random random) {
    this.random = random;
  }

  private void play() {
    var in = new Scanner(System.in);
    var state = new GameState(START_ROOM, START_WUMPUS, START_ARROWS, 0);

    System.out.println("Hunt the Wumpus");
    System.out.println("Torus rules: 100 cells, 10 pits, 10 bats, and a Wumpus.");
    System.out.println(COMMAND_HELP);

    while (true) {
      if (BATS.contains(state.playerRoom())) {
        System.out.println("Super bats whisk you to another room!");
        state = new GameState(randomRoom(), state.wumpusRoom(), state.arrows(), state.rotation());
        continue;
      }

      var outcome = roomOutcome(state);
      if (outcome != null) {
        System.out.println(outcome);
        return;
      }

      printRoom(state);
      System.out.printf("Arrows: %d | command: move <room>, shoot <r1> <r2> ... <r5>, status, help, quit%n",
          state.arrows());
      if (!in.hasNextLine()) {
        return;
      }

      var line = in.nextLine().trim();
      if (line.isEmpty()) {
        System.out.println("Unknown command.");
        continue;
      }

      var parts = line.split("\\s+");
      var verb = parts[0];

      if ("quit".equals(verb)) {
        System.out.println("Goodbye.");
        return;
      }

      if ("help".equals(verb)) {
        System.out.println(COMMAND_HELP);
        continue;
      }

      if ("status".equals(verb)) {
        System.out.printf("Status: room=%s, arrows=%d, rotation=%d, tunnels=%s%n",
            state.playerRoom(), state.arrows(), state.rotation(), neighbors(state.playerRoom(), state.rotation()));
        continue;
      }

      if ("move".equals(verb)) {
        if (parts.length < 2) {
          System.out.println("You must name an adjacent room.");
          continue;
        }
        var destination = parseRoom(parts[1]);
        if (destination == null || !adjacent(state.playerRoom(), destination, state.rotation())) {
          System.out.println("You can only move to an adjacent room.");
          continue;
        }
        state = rotate(new GameState(applyBats(destination), state.wumpusRoom(), state.arrows(), state.rotation()));
        System.out.printf("You move to room %s%n", destination);
        continue;
      }

      if ("shoot".equals(verb)) {
        if (parts.length < 2) {
          System.out.println("Provide one to five rooms for the arrow path.");
          continue;
        }

        var path = new ArrayList<String>();
        for (int i = 1; i < parts.length && path.size() < 5; i++) {
          var room = parseRoom(parts[i]);
          if (room != null) {
            path.add(room);
          }
        }

        if (path.isEmpty()) {
          System.out.println("Provide one to five rooms for the arrow path.");
          continue;
        }

        var shot = shoot(state, path);
        if (shot == ShotResult.WON) {
          System.out.println("Your arrow strikes true. You win!");
          return;
        }
        if (shot == ShotResult.LOST_SELF) {
          System.out.println("The crooked arrow returns to your room. You lose.");
          return;
        }

        System.out.println("Your arrow misses.");
        var nextArrows = state.arrows() - 1;
        var rotatedState = rotate(state);
        var nextWumpus = moveWumpus(rotatedState.wumpusRoom(), rotatedState.rotation());
        if (nextWumpus.equals(rotatedState.playerRoom())) {
          System.out.println("The Wumpus wakes, moves, and eats you.");
          return;
        }
        if (nextArrows == 0) {
          System.out.println("You are out of arrows. You lose.");
          return;
        }
        state = new GameState(rotatedState.playerRoom(), nextWumpus, nextArrows, rotatedState.rotation());
        continue;
      }

      System.out.println("Unknown command.");
    }
  }

  private String roomOutcome(GameState state) {
    if (state.playerRoom().equals(state.wumpusRoom())) {
      return "You enter the Wumpus room. You lose.";
    }
    if (PITS.contains(state.playerRoom())) {
      return "You fall into a bottomless pit. You lose.";
    }
    return null;
  }

  private void printRoom(GameState state) {
    System.out.println();
    System.out.printf("You are in room %s%n", state.playerRoom());
    System.out.printf("Tunnels lead to %s, %s, %s, %s, %s%n",
        neighbors(state.playerRoom(), state.rotation()).get(0),
        neighbors(state.playerRoom(), state.rotation()).get(1),
        neighbors(state.playerRoom(), state.rotation()).get(2),
        neighbors(state.playerRoom(), state.rotation()).get(3),
        neighbors(state.playerRoom(), state.rotation()).get(4));
    if (adjacent(state.playerRoom(), state.wumpusRoom(), state.rotation())) {
      System.out.println("You smell a Wumpus.");
    }
    if (neighbors(state.playerRoom(), state.rotation()).stream().anyMatch(PITS::contains)) {
      System.out.println("You feel a draft.");
    }
    if (neighbors(state.playerRoom(), state.rotation()).stream().anyMatch(BATS::contains)) {
      System.out.println("You hear rustling bats.");
    }
  }

  private ShotResult shoot(GameState state, List<String> path) {
    String arrowRoom = state.playerRoom();
    for (String requested : path) {
      arrowRoom = adjacent(arrowRoom, requested, state.rotation()) ? requested : randomNeighbor(arrowRoom, state.rotation());
      if (arrowRoom.equals(state.wumpusRoom())) {
        return ShotResult.WON;
      }
      if (arrowRoom.equals(state.playerRoom())) {
        return ShotResult.LOST_SELF;
      }
    }
    return ShotResult.MISS;
  }

  private String applyBats(String room) {
    return BATS.contains(room) ? randomRoom() : room;
  }

  private String moveWumpus(String room, int rotation) {
    int roll = random.nextInt(4);
    return roll == 0 ? room : neighbors(room, rotation).get(roll - 1);
  }

  private String randomRoom() {
    return makeRoom(random.nextBoolean() ? 'O' : 'I', random.nextInt(CELL_COUNT));
  }

  private String randomNeighbor(String room, int rotation) {
    var exits = neighbors(room, rotation);
    return exits.get(random.nextInt(exits.size()));
  }

  private static List<String> neighbors(String room, int rotation) {
    int index = roomIndex(room);
    int row = index / WIDTH;
    int col = index % WIDTH;
    char torus = roomTorus(room);
    return List.of(
        makeRoom(torus, cell(row, col + 1)),
        makeRoom(torus, cell(row + 1, col)),
        makeRoom(torus, cell(row, col - 1)),
        makeRoom(torus, cell(row - 1, col)),
        bridgeRoom(room, rotation));
  }

  private static boolean adjacent(String from, String to, int rotation) {
    return neighbors(from, rotation).contains(to);
  }

  private static String parseRoom(String token) {
    if (token == null || token.length() < 2) {
      return null;
    }
    char prefix = Character.toUpperCase(token.charAt(0));
    if (prefix != 'O' && prefix != 'I') {
      return null;
    }
    try {
      int room = Integer.parseInt(token.substring(1));
      return room >= 0 && room < CELL_COUNT ? makeRoom(prefix, room) : null;
    } catch (NumberFormatException ignored) {
      return null;
    }
  }

  private static int cell(int row, int col) {
    return Math.floorMod(row, WIDTH) * WIDTH + Math.floorMod(col, WIDTH);
  }

  private static char roomTorus(String room) {
    return room.charAt(0);
  }

  private static int roomIndex(String room) {
    return Integer.parseInt(room.substring(1));
  }

  private static String makeRoom(char torus, int index) {
    return torus + Integer.toString(index);
  }

  private static String bridgeRoom(String room, int rotation) {
    int index = roomIndex(room);
    if (roomTorus(room) == 'O') {
      return makeRoom('I', Math.floorMod(index + rotation, CELL_COUNT));
    }
    return makeRoom('O', Math.floorMod(index - rotation, CELL_COUNT));
  }

  private static GameState rotate(GameState state) {
    return new GameState(state.playerRoom(), state.wumpusRoom(), state.arrows(),
        Math.floorMod(state.rotation() + WIDTH, CELL_COUNT));
  }

  private enum ShotResult {
    WON,
    LOST_SELF,
    MISS
  }

  private static Random makeRandom(String[] args) {
    if (args.length == 0) {
      return new Random();
    }
    try {
      return new Random(Long.parseLong(args[0]));
    } catch (NumberFormatException ignored) {
      return new Random();
    }
  }
}
