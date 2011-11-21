
import java.sql.SQLException;
import java.util.*;

public class Experiment {

	Integer experiment_id;
	final String username;
	final String datetime;
	final String description;
	
	public Experiment(String user, String dtime, String desc) {
		if (user == null)
			throw new NullPointerException("user");
		if (dtime == null)
			throw new NullPointerException("dtime");
		if (desc == null)
			throw new NullPointerException("desc");
		experiment_id = null;
		username = user;
		datetime = dtime;
		description = desc;
	}

	int writeToDB(int problem_id) throws ClassNotFoundException, SQLException {
		// SIDE EFFECT: sets this.experiment_id to the new key
		// SIDE EFFECT: returns the new key
		String[] fieldNames = {"problem_id", "username", "datetime", "description"};
		String[] values = {String.valueOf(problem_id), username, datetime, description};
		int k = Utils.insertAndReturnNewKey("experiments", fieldNames, values, "experiment_id");
		this.experiment_id = k;
		return k;
	}
	
}


