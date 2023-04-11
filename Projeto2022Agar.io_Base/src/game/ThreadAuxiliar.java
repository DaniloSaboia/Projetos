package game;

import environment.Cell;

public class ThreadAuxiliar extends Thread {
	Cell x;

	public ThreadAuxiliar(Cell x) {
		this.x = x;


	}

	@Override
	public void run ()  {
		x.lock1.lock();
		try {
			sleep(2000);
			x.cond.signal();
		} catch (InterruptedException e) {
			interrupt();
			e.printStackTrace();
		}finally {
			x.lock1.unlock();
		}
	}


}
