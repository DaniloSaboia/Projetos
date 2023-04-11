package gui;

import java.net.InetAddress;
import java.net.Socket;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.PrintWriter;
import java.io.OutputStreamWriter;

public class Client {

	private Socket socket;
	private ObjectInputStream input;
	private PrintWriter output;
	private int port;
	private String address;
	private GameClient game;
	private ClientGuiMain gui;

	public Client(boolean alternativeKeys, int port) {
		this.gui = new ClientGuiMain(alternativeKeys);
		this.port = port;
		this.game = gui.game;
		gui.init();
	}

	public void startClient() throws ClassNotFoundException {
		try {
			InetAddress inet = InetAddress.getByName(address);
			socket = new Socket(inet, port);
			input = new ObjectInputStream(socket.getInputStream());
			output = new PrintWriter(new BufferedWriter(new OutputStreamWriter(socket.getOutputStream())), true);

			while (true) {
				message msg = (message) input.readObject();
				game.msg = msg;
				game.notifyChange();

				while (gui.boardGui.getLastPressedDirection() != null) {
					output.println(gui.boardGui.getLastPressedDirection().toString());
					gui.boardGui.clearLastPressedDirection();
				}

				if (msg.end) {
					break;
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			try {
				socket.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	public static void main(String[] args) throws ClassNotFoundException {
		Client client = new Client(false, 12345);
		client.startClient();
	}
}
