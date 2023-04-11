package gui;

import java.util.Observable;
import java.util.Observer;

import javax.swing.JFrame;

public class ClientGuiMain implements Observer {

	private JFrame frame = new JFrame("Agar.io");
	public BoardJComponentClient boardGui;
	private boolean alternativeKeys;
	public GameClient game;

	public ClientGuiMain(boolean alternativeKeys) {
		super();
		this.alternativeKeys = alternativeKeys;
		game = new GameClient(null);
		game.addObserver(this);
		buildGui();
	}

	private void buildGui() {
		boardGui = new BoardJComponentClient(game, alternativeKeys);
		frame.add(boardGui);
		frame.setSize(800, 800);
		frame.setLocation(0, 150);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}

	@Override
	public void update(Observable o, Object arg) {
		// TODO Auto-generated method stub
		boardGui.repaint();
	}

	public void init() {
		frame.setVisible(true);
	}

	public static void main(String[] args) {
		ClientGuiMain game = new ClientGuiMain(false);
		game.init();
	}
}
