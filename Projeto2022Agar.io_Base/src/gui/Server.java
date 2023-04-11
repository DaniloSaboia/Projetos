package gui;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.ObjectOutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.CountDownLatch;
import game.Game;
import game.RealPlayer;

public class Server extends Thread {

	public static final int PORT = 8080;
	private Game game;
	private CountDownLatch countdown;
	public static final int Delay = 0;
	private int player;
	private BufferedReader input;
	private ObjectOutputStream output;
	private RealPlayer realPlayer;
	private message content;

	public Server(Game game) {
		this.game = game;
		this.countdown = game.getCountDownLatch();
		player = 1;
	}

	private environment.Direction toDir(String dir) {

		environment.Direction pos = null;

		switch (dir) {

		case "UP":
			pos = environment.Direction.UP;
			break;
		case "DOWN":
			pos = environment.Direction.DOWN;
			break;
		case "LEFT":
			pos = environment.Direction.LEFT;
			break;
		case "RIGHT":
			pos = environment.Direction.RIGHT;
			break;
		}

		return pos;
	}

	private void GameStateSender() throws InterruptedException {
		// message content;

		while (!game.isOver()) {
			try {
				content = new message(game);
				output.writeObject(content);
				output.flush();
				output.reset();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}	
	
	@Override
	public void run() {
		ServerSocket s;
		try {
			s = new ServerSocket(12345);
			try {
				while (true) {
					Socket socket = s.accept();
					System.out.println("Server na socket: " + socket.toString());
					// input e output do Server
					input = new BufferedReader(new InputStreamReader(socket.getInputStream()));
					output = new ObjectOutputStream(socket.getOutputStream());
					try {
						realPlayer = new RealPlayer(Game.NUM_PLAYERS + player++, game, (byte) 5, countdown);
						realPlayer.start();
						GameStateSender();
						String dirClient = input.readLine();
						System.out.println("last Direction: " + dirClient);
						realPlayer.setLastPressedDirection(toDir(dirClient));
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				}
			} finally {
				input.close();
				output.close();
				System.out.println("Connection stopped");
				s.close();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static void main(String[] args) {
		GameGuiMain game = new GameGuiMain();
		new Server(game.getGame()).start();
		game.init();
	}

}
