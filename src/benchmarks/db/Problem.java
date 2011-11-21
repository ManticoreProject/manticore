public class Problem {

	Integer problem_id;
	final String problem_name;

	public Problem(String name) {
		if (name == null)
			throw new NullPointerException();
		problem_id = null;
		problem_name = name;
	}

}

