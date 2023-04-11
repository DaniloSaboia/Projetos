package game;

import java.util.concurrent.CountDownLatch;

/**
 * Class to demonstrate a player being added to the game.
 * 
 * @author luismota
 *
 */
public class PhoneyHumanPlayer extends Player {
	public PhoneyHumanPlayer(int id, Game game, byte strength,CountDownLatch latch) {
		super(id, game, strength,latch);
	}

	public boolean isHumanPlayer() {
		return false;
	}

}
