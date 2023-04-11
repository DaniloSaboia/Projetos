package gui;

import java.util.Observable;

public class GameClient extends Observable{
	
	public message msg;
	
	public GameClient(message msg) {
		this.msg = msg;
	}
	
	public void notifyChange() {
		setChanged();
		notifyObservers();
	}
}
