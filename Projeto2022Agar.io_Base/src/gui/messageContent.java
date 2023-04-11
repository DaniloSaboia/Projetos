package gui;

import java.io.Serializable;
import environment.Coordinate;

public class messageContent implements Serializable {

	public int strength;
	public Coordinate coord;
	public boolean isHuman;
	public int id;

	public messageContent(int strength, Coordinate coord, boolean human, int id) {

		this.coord = coord;
		this.strength = strength;
		this.isHuman = human;
		this.id = id;

	}
}
