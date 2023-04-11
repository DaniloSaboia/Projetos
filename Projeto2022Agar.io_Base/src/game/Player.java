package game;

import java.lang.Thread.State;
import java.util.List;
import java.util.Random;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

import environment.Cell;
import environment.Coordinate;
import environment.Direction;

/**
 * Represents a player.
 * 
 * @author luismota
 *
 */

public abstract class Player extends Thread {

	protected Game game;
	private int id;
	private byte currentStrength;
	protected byte originalStrength;
	ReentrantLock lock = new ReentrantLock();
	ReentrantLock lockThreadAux = new ReentrantLock();
	protected Condition cond = lockThreadAux.newCondition();
	private CountDownLatch latch;


	public Player(int id, Game game, byte strength,CountDownLatch latch) {
		super();
		this.id = id;
		this.game = game;
		currentStrength = strength;
		originalStrength = strength;
		this.latch = latch;

	}

	public abstract boolean isHumanPlayer();

	@Override
	public String toString() {
		return "Player [id=" + id + ", currentStrength=" + currentStrength + ", getCurrentCell()=" + getCurrentCell()
		+ "]";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + id;
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Player other = (Player) obj;
		if (id != other.id)
			return false;
		return true;
	}

	public byte getCurrentStrength() {
		return currentStrength;
	}

	public byte SetCurrentStrength0(byte x) {
		return currentStrength = x;
	}

	public int getIdentification() {
		return id;
	}

	// feito por nos
	// TODO: get player position from data in game

	// --------------------------------------------//
	// Condiçoes//
	public boolean canMove(Coordinate c) {
		if (c.x < 0)
			return false;
		if (c.y < 0)
			return false;
		if (c.x >= 30)
			return false;
		if (c.y >= 30)
			return false;

		return true;

	}

	public int fix(int x) {
		if (x > 10) {
			return x = 10;
		} else {
			return x;
		}
	}

	public boolean isPlayerAlive() {
		if (currentStrength > 0 && currentStrength < 10) {
			return true;
		}
		return false;
	}

	public void speedCondition() {
		try {
			if (originalStrength == 3)
				Thread.sleep(1200);
			if (originalStrength == 2)
				Thread.sleep(800);
			if (originalStrength == 1 || isHumanPlayer())
				Thread.sleep(400);
		} catch (InterruptedException e) {
			interrupt();
			e.printStackTrace();
		}
	}

	public void StartOfTheGame() {
		Cell initialPos = game.getRandomCell();
		initialPos.insertPlayersInGame(this);
		game.incrementNumPlayers();

//		try {
//			sleep(game.INITIAL_WAITING_TIME);
//		} catch (InterruptedException e) {
//			e.printStackTrace();
//		}
	}

	public void checkifCellLocked(Cell x) {
		if (x.isItLocked()) {
			try {
				wait();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}

	public boolean isCellLocked(Cell x) {
		if (x.isItLocked()) {
			return true;

		}
		return false;
	}

	// ------------------------------------------------------//

	public Cell getCurrentCell() {
		Cell[][] board = game.board;
		for (int i = 0; i != 30; i++) {
			for (int j = 0; j != 30; j++) {
				if (this.equals(board[i][j].getPlayer())) {
					return board[i][j];
				}
			}
		}
		return null;
	}

	public void VerificacaoAndSetPlayers(Coordinate x) {
		Cell cellmove = game.getCell(x);
		cellmove.setLockCell();
		if (cellmove.isOcupied()) {
			if (cellmove.getPlayer().isPlayerAlive()) {          
				Fight(cellmove.getPlayer());
			} else {
				cellmove.Auxiliar();
			}
		} else {
			this.getCurrentCell().RemovePlayer();             //lock na cell atual tbm, synchronized + lock = X
			Cell novaPos = game.getCell(cellmove.getPosition());
			novaPos.setPlayer(this);
		}
		cellmove.unlockCell();
	}


	public void antiCopypast(Coordinate x) {
		while (game.getCell(x).isItLocked() ) {
			try {
				wait();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		VerificacaoAndSetPlayers(x);
		notifyAll();
	}

	public synchronized void moveAuto() {

		if (!this.isHumanPlayer()) {
			if (getCurrentCell() != null) {
				if (getCurrentCell().getPosition() != null) {
					Coordinate x = this.getCurrentCell().getPosition();
					Cell pos = game.getCell(x);
					Random rand = new Random();
					int y = rand.nextInt(4);

					switch (y) {
					case 0:
						if (canMove(pos.getPosition().translate(Direction.UP.getVector()))) {
							Coordinate novo = pos.getPosition().translate(Direction.UP.getVector());
							antiCopypast(novo);
						}
						break;

					case 1:

						if (canMove(pos.getPosition().translate(Direction.LEFT.getVector()))) {
							Coordinate novo1 = pos.getPosition().translate(Direction.LEFT.getVector());
							antiCopypast(novo1);

						}
						break;

					case 2:
						if (canMove(pos.getPosition().translate(Direction.RIGHT.getVector()))) {
							Coordinate novo2 = pos.getPosition().translate(Direction.RIGHT.getVector());
							antiCopypast(novo2);
						}

						break;

					case 3:
						if (canMove(pos.getPosition().translate(Direction.DOWN.getVector()))) {
							Coordinate novo3 = pos.getPosition().translate(Direction.DOWN.getVector());
							antiCopypast(novo3);
						}

						break;

					}

				}
			}
		}

	}



	public void move(Direction D) {
		if (this.getCurrentCell().getPosition() != null) {
			Coordinate x = new Coordinate(this.getCurrentCell().getPosition().x + D.getVector().x,
					this.getCurrentCell().getPosition().y + D.getVector().y);
			if (this.canMove(x)) {
				VerificacaoAndSetPlayersReais(x);
			}
		}
	}
	public void VerificacaoAndSetPlayersReais(Coordinate x) {
		Cell cellmove = game.getCell(x);
		cellmove.setLockCell();
		if (cellmove.isOcupied()) {
			if (cellmove.getPlayer().isPlayerAlive()) {
				Fight(cellmove.getPlayer());
			} 
		} else {
			this.getCurrentCell().RemovePlayer();
			Cell novaPos = game.getCell(cellmove.getPosition());
			novaPos.setPlayer(this);

		}
		cellmove.unlockCell();
	}

	public Player Fight(Player p) {
		lock.lock();
		if (p != null && p.isPlayerAlive()) {

			if (p.getCurrentStrength() < currentStrength) { // player p mais fraco que player atual
				int sum = currentStrength + p.getCurrentStrength(); // player atual fica com a força dos dois somada
				int finalSum = fix(sum);
				currentStrength = (byte) finalSum; // player p morre
				if(currentStrength== 10) {
					latch.countDown();
				}
				p.SetCurrentStrength0((byte) 0);
				lock.unlock();
				return this;

			} else if (p.getCurrentStrength() == getCurrentStrength()) {

				Random rand = new Random();
				int y = rand.nextInt(2);
				// ha 2 hipoteses quando morre tem a mesma vida
				switch (y) {

				case 0:
					int sum = currentStrength + p.getCurrentStrength();
					int finalSum = fix(sum);
					currentStrength = (byte) finalSum;
					if(currentStrength== 10) {
						latch.countDown();
					}
					p.SetCurrentStrength0((byte) 0);
					lock.unlock();
					return this;

				case 1:
					int sum3 = currentStrength + p.getCurrentStrength();
					int finalSum1 = fix(sum3);
					p.currentStrength = (byte) finalSum1;
					if(p.currentStrength== 10) {
						latch.countDown();
					}
					SetCurrentStrength0((byte) 0);
					lock.unlock();
					return p;
				}
			} else {
				int sum = currentStrength + p.getCurrentStrength();
				int finalSum = fix(sum);
				p.currentStrength = (byte) finalSum; //// player p mais forte que player atual ,player atual morre
				if(p.currentStrength== 10) {
					latch.countDown();
				}
				SetCurrentStrength0((byte) 0);
				lock.unlock();
				return p; // player p fica com a força dos dois somada
			}
		}
		lock.unlock();
		return null;

	}

	@Override
	public synchronized void run() {
		StartOfTheGame();
		while (isPlayerAlive() && !isInterrupted()) {
			if (!isHumanPlayer()) {
				moveAuto();
				game.notifyChange();
				speedCondition();
			} else {
				
			}


		}
	}
}




	

