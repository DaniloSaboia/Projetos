package game;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Observable;
import java.util.Random;
import java.util.concurrent.CountDownLatch;
import environment.Cell;
import environment.Coordinate;


public class Game extends Observable implements Serializable {

	public static final int DIMY = 30;
	public static final int DIMX = 30;
	public static final int NUM_PLAYERS = 90;
	private static int CURRENT_NUM_PLAYERS = 0;
	private static final int NUM_FINISHED_PLAYERS_TO_END_GAME = 3;
	private static int NUM_FINISHED_PLAYERS = 0;
	public static final long REFRESH_INTERVAL = 400;
	public static final double MAX_INITIAL_STRENGTH = 3;
	public static final long MAX_WAITING_TIME_FOR_MOVE = 2000;
	public static final long INITIAL_WAITING_TIME = 10000;
	CountDownLatch latch = new CountDownLatch(NUM_FINISHED_PLAYERS_TO_END_GAME);
	public List<Thread> Listat = new ArrayList<Thread>();
	boolean isOver = false;




	protected Cell[][] board;
	

	public Game() {
		board = new Cell[Game.DIMX][Game.DIMY];

		for (int x = 0; x < Game.DIMX; x++)
			for (int y = 0; y < Game.DIMY; y++)
				board[x][y] = new Cell(new Coordinate(x, y), this);
	}

	public void notifyChange() {
		setChanged();
		notifyObservers();
	}

	//funçoes implementadas por nos 


	//------------------gets---------------------//
	
	public boolean isOver() {
		return isOver;
	}
	
	public CountDownLatch getCountDownLatch() {
		return latch;
	}

	public List<Thread> getListaThreads() {
		return Listat;
	}

	public Cell [][] getBoard() {
		return board;
	}


	public int getNumFinishedPlayers() {
		return NUM_FINISHED_PLAYERS;
	}

	public int getCurrentNumPlayers() {
		return CURRENT_NUM_PLAYERS;
	}

	public List<Player> getListAllPlayers() {
		List<Player> novaLista = new ArrayList<Player>();
		for (int x = 0; x < Game.DIMX; x++) {
			for (int y = 0; y < Game.DIMY; y++) {
				if (board[x][y].isOcupied()) {
					novaLista.add(board[x][y].getPlayer());
				}
			}
		}
		return novaLista;
	}

	public Cell getRandomCell() {
		Cell newCell = getCell(new Coordinate((int) (Math.random() * Game.DIMX), (int) (Math.random() * Game.DIMY)));
		return newCell;
	}


	public Cell getCell(Coordinate at) {
		return board[at.x][at.y];
	}
	//-------------------------------------------//
	//------------Incrementaçoes---------------//

	public void incrementNumPlayers() {
		CURRENT_NUM_PLAYERS++;
	}

	//----------------------------------------//
	//------------CriaçaoThreads---------------//

	public void criarBots() {


		for (int i = 1; i <= NUM_PLAYERS; i++) {

			Random rand = new Random();
			int y = rand.nextInt(3) + 1;
			Player player = new PhoneyHumanPlayer(i, this, (byte) y,latch);
			Thread t = new Thread(player);
			t.start();
			Listat.add(t);

		}
		try {
			latch.await();
		} catch (InterruptedException e) {
			
			e.printStackTrace();
		}

		for(Thread threads: Listat) {
			threads.stop();
		}
		isOver=true;
	}





	public void criaPlayers() {

		Player player = new RealPlayer(getCurrentNumPlayers()+1,this,(byte) 5,latch);
		Thread t = new Thread(player);
		t.start();
		Listat.add(t);
		try {
			latch.await();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		isOver=true;
	}
	//----------------------------------------//


}
