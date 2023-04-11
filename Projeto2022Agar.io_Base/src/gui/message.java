package gui;

import java.io.Serializable;
import java.util.ArrayList;

import game.Game;
import game.Player;

public class message implements Serializable {

	int size = 0;
	ArrayList<messageContent> toClient = new ArrayList<messageContent>(size);
	public final static int y = Game.DIMY;
	public final static int x = Game.DIMX;
	public boolean end;

	public message(Game game) {

		this.end = game.isOver();
		ArrayList<Player> player = (ArrayList<Player>) game.getListAllPlayers();
		this.size = player.size();

		for (int i = 0; i < this.size && player.get(i).getCurrentCell() != null; i++) {

			messageContent novo = new messageContent(player.get(i).getCurrentStrength(),
					player.get(i).getCurrentCell().getPosition(), player.get(i).isHumanPlayer(),
					player.get(i).getIdentification());
			toClient.add(i, novo);
		}
	}
}
