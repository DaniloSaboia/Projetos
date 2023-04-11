package environment;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

import game.Game;
import game.Player;
import game.ThreadAuxiliar;

public class Cell{
	private Coordinate position;
	private Game game;
	private Player player = null;
	ReentrantLock lock = new ReentrantLock();
	public ReentrantLock lock1 = new ReentrantLock();
	public Condition cond = lock1.newCondition();

	public Cell(Coordinate position, Game g) {
		super();
		this.position = position;
		this.game = g;
	}

	public synchronized Coordinate getPosition() {
		return position;
	}

	public boolean isOcupied() {
		return player != null;
	}

	public Player getPlayer() {
		if (player != null) {
			return player;
		} else {
			return null;
		}
	}

	public void setLockCell() {
		lock.lock();
	}

	public void unlockCell() {
		lock.unlock();
	}

	public boolean isItLocked() {
		if (lock.isLocked()) {
			return true;
		}
		return false;
	}

	public synchronized void RemovePlayer() {
		this.player = null;
		notifyAll();
	}
	
	public synchronized void Auxiliar() {
		ThreadAuxiliar t = new ThreadAuxiliar(this);
		t.start();
		game.getListaThreads().add(t);
		lock1.lock();
		try {
			cond.await();		
		} catch (InterruptedException e) {
			e.printStackTrace();
		}finally{ lock1.unlock(); 
		}
	}

	public synchronized void insertPlayersInGame(Player player) {  //starvation inserir noutra cell caso esteja morta
		while (isOcupied()) {
			if(this.getPlayer().getCurrentStrength() == 0) {
				break;
			}
			try {
				wait();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		if(!isOcupied()) {
			this.player = player;
		}
		
	}

	public synchronized void setPlayer(Player player) {
			this.player = player;
		
	}
}