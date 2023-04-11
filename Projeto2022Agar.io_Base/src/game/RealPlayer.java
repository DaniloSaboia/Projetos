package game;

import java.util.concurrent.CountDownLatch;

import environment.Direction;

/**
 * Class to demonstrate a player being added to the game.
 * 
 * @author luismota
 *
 */
public class RealPlayer extends Player {
	
	private volatile Direction lastPressedDirection;
	
	public RealPlayer(int id, Game game, byte strength,CountDownLatch latch) {
		super(id, game, strength,latch);
		lastPressedDirection = null;
	}

	public boolean isHumanPlayer() {
		return true;
	}
	
	public void setLastPressedDirection(Direction direction) {
		lastPressedDirection = direction;
	}

	public Direction getLastPressedDirection() {
		return lastPressedDirection;
	}

	public void setDirectionNull() {
		lastPressedDirection = null;
	}

}
