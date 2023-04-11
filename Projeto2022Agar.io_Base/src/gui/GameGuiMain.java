package gui;

import java.util.Observable;
import java.util.Observer;
import game.Game;
import javax.swing.JFrame;

public class GameGuiMain implements Observer {
	private JFrame frame = new JFrame("pcd.io");
	private BoardJComponent boardGui;
	private Game game;

	public GameGuiMain() {
		super();
		game = new Game();
		game.addObserver(this);
		buildGui();
	}

	public Game getGame() {
		return this.game;
	}

	private void buildGui() {
		boardGui = new BoardJComponent(game, false);
		frame.add(boardGui);
		frame.setSize(800, 800);
		frame.setLocation(0, 150);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}

	public void init() {
		frame.setVisible(true);
		game.criarBots();
	}

	@Override
	public void update(Observable o, Object arg) {
		boardGui.repaint();
	}

	public static void main(String[] args) {
		GameGuiMain game = new GameGuiMain();
		game.init();
	}
}
