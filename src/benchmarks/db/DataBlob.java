import java.util.*;
import java.sql.*;
import java.io.*;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

class AlreadyInDB extends RuntimeException {
    public AlreadyInDB() {
	super();
    }
    public AlreadyInDB(String s) {
	super(s);
    }
}

public class DataBlob {

    Problem p;
    Experiment e;
    Context c;
    List<Run> runs;

    public DataBlob(Problem p,
		    Experiment e,
		    Context c,
		    List<Run> runs) {
	if (e == null)
	    throw new NullPointerException("e");
	if (c == null)
	    throw new NullPointerException("c");
	if (runs == null)
	    throw new NullPointerException("runs");
	int nRuns = runs.size();
	if (nRuns == 0)
	    System.out.println("WARNING: zero-length list of runs. Check the current data file.");
	this.p = p;
	this.e = e;
	this.c = c;
	// make defensive copy of runs
	this.runs = new ArrayList<Run>(nRuns);
	for (Run r : runs)
	    this.runs.add(r); // may or may not be stable wrt order, but that doesn't matter
    }
	
    void writeToDB() throws ClassNotFoundException, SQLException {	

	if (this.runs.size() == 0) {
	    System.out.println("empty run list; not writing anything from " + 
			       this.c.data_source_file);
	    return;
	}
	// find or create problem_id
	int problem_id = Utils.findByLookupOrInsert("problems",
						    "problem_name",
						    p.problem_name,
						    "problem_id");
	// System.out.println("problem_id: " + problem_id);
	// insert experiment record with given problem_id		
	int experiment_id = e.writeToDB(problem_id);	
	System.out.println("experiment_id: " + experiment_id);
	writeToDB(experiment_id);		
    }
	
    void writeToDB(int experiment_id) throws ClassNotFoundException, SQLException {
	// System.out.println("experiment_id: " + experiment_id);
	// insert context record with given experiment_id
	int context_id = c.writeToDB(experiment_id);
	// insert run records with given context_id
	for (Run r : this.runs) {
	    int throwAway = r.writeToDB(context_id);
	}
    }

    static DataBlob fromJSON(File f) 
	throws ClassNotFoundException, SQLException, RuntimeException, IOException, JSONException {
	DataBlob b = DataBlob.fromJSON(f.getAbsolutePath());
	return b;
    }

    static DataBlob fromJSON(String filename) 
	throws ClassNotFoundException, SQLException, RuntimeException, IOException, JSONException {

	// first check if the current source file has already been pushed into the db
	// if so, return null
	String justFile = (new File(filename)).getName();

	Integer kOpt = Utils.lookFor("contexts",
				     "data_source_file",
				     justFile,
				     "context_id");

	if (kOpt != null) {
// 	    System.out.println("With respect to data file " + justFile + ":");
// 	    System.out.println("  - It looks as though this file has already been recorded in the db.");
// 	    System.out.println("  - Skipping it and moving on to the next file.");
	    throw new AlreadyInDB(justFile);
	}

	// since the file looks not to be in the database, parse it and write it
	BufferedReader br = new BufferedReader(new FileReader(filename));
	String in = "";
	while (br.ready()) {
	    in += br.readLine();
	}
	br.close();

	JSONObject j = new JSONObject(in);

	// get all the mandatory items
	String problem_name = j.getString("problem_name");
	String username = j.getString("username");
	String datetime = j.getString("datetime");
	String language = j.getString("language");
	String compiler = j.getString("compiler");
	String version = j.getString("version");
	String description = j.getString("description");
	String bench_url = j.getString("bench_url");
	int bench_svn = j.getInt("bench_svn");
	String input = j.getString("input");
	String machine = j.getString("machine");		
	JSONArray runs = j.getJSONArray("runs");

	Problem p = new Problem(problem_name);
		
	Experiment e = new Experiment(username, datetime, description);
		
	Context c = new Context(language, compiler, version, bench_url, bench_svn,
				input, username, machine, datetime, justFile);

	int nRuns = runs.length();
	List<Run> rs = new ArrayList<Run>(nRuns);
	for (int i = 0; i < nRuns; i++) {

	    JSONObject curr = runs.getJSONObject(i);
	    int n_procs = curr.getInt("n_procs");
	    double time_sec = curr.getDouble("time_sec");
	    // TODO -- cpu time and max bytes

	    List<GC> gcs = new ArrayList<GC>(0);

	    if (curr.has("gc")) {

		JSONArray jgcs = curr.getJSONArray("gc");
		int ngcs = jgcs.length();
		for (int k = 0; k < ngcs; k++) {
		    JSONObject currGC = jgcs.getJSONObject(k);
		    int processor = currGC.getInt("processor");

		    // there might be a field "elapsed_time_sec" containing a double
		    Double elapsed_time_sec = null;

		    if (currGC.has("elapsed_time_sec")) {
			elapsed_time_sec = currGC.getDouble("elapsed_time_sec");
		    }

		    JSONObject minor = currGC.getJSONObject("minor");
	
		    int minor_n_collections    = minor.getInt("num");
		    long minor_alloc_bytes     = minor.getLong("alloc");
		    long minor_collected_bytes = minor.getLong("collected");
		    long minor_copied_bytes    = minor.getLong("copied");
		    double minor_time_coll_sec = minor.getDouble("time");

		    JSONObject major = currGC.getJSONObject("major");
		    int major_n_collections    = major.getInt("num");
		    long major_alloc_bytes     = major.getLong("alloc");
		    long major_collected_bytes = major.getLong("collected");
		    long major_copied_bytes    = major.getLong("copied");
		    double major_time_coll_sec = major.getDouble("time");

		    JSONObject global = currGC.getJSONObject("global");
		    int global_n_collections    = global.getInt("num");
		    long global_alloc_bytes     = global.getLong("alloc");
		    long global_collected_bytes = global.getLong("collected");
		    long global_copied_bytes    = global.getLong("copied");
		    double global_time_coll_sec = global.getDouble("time");

		    JSONObject promotion = currGC.getJSONObject("promotion");
		    int n_promotions          = promotion.getInt("num");
		    long prom_bytes           = promotion.getLong("bytes");
		    double mean_prom_time_sec = promotion.getDouble("time");

		    GC gc = new GC(processor,
				   minor_n_collections,
				   minor_alloc_bytes,
				   minor_collected_bytes,
				   minor_copied_bytes,
				   minor_time_coll_sec,
				   major_n_collections,
				   major_alloc_bytes,
				   major_collected_bytes,
				   major_copied_bytes,
				   major_time_coll_sec,
				   global_n_collections,
				   global_alloc_bytes,
				   global_collected_bytes,
				   global_copied_bytes,
				   global_time_coll_sec,
				   n_promotions,
				   prom_bytes,
				   mean_prom_time_sec,
				   elapsed_time_sec);
		    gcs.add(gc);
		}
	    }

	    Run r = new Run(n_procs, time_sec, gcs);
	    rs.add(r);
	}
		
	// get the optional Manticore-only items
	// TODO using presence of one key (compiler_src_url) to
	//      determine if all Manticore items are present...
	//      think of a better way
	if (j.has("compiler_src_url")) {
	    String compiler_src_url = j.getString("compiler_src_url");
	    int compiler_svn = j.getInt("compiler_svn");
	    String script_url = j.getString("script_url");
	    int script_svn = j.getInt("script_svn");
	    boolean seq_compilation = j.getBoolean("seq_compilation");
	    int max_leaf_size = j.optInt("max_leaf_size");
	    int seq_cutoff = j.optInt("seq_cutoff");
	    c.setManticoreInfo(compiler_src_url, 
			       compiler_svn, 
			       script_url, 
			       script_svn, 
			       seq_compilation,
			       max_leaf_size, 
			       seq_cutoff);
	}

	DataBlob b = new DataBlob(p, e, c, rs);
		
	return b;
    }
	
    static void writeDirToDB(String dirname) 
	throws ClassNotFoundException, SQLException, IOException, JSONException {
	File dir = new File(dirname);
	File[] contents = dir.listFiles();
	if (contents.length == 0) {
	    System.out.println("WARNING: no files in " + dirname + "; continuing");
	    return;
	}
	List<File> jsons = new ArrayList<File>(contents.length);
	// traverse dir
	for (File f : contents) {
	    if (f.isDirectory()) {
		System.out.println("- UNEXPECTED dir found within " + dirname);
	    } else if (f.isFile()) {
		// System.out.println("- will process " + f + " in " + dirname);
		if (f.getName().endsWith(".json")) {
		    jsons.add(f);
		} else {
		    System.out.println("skipping " + f.getName() + " in " + dirname);
		}
	    } else {
		System.out.println("- " + f + " is not a dir and not a file(?)");
	    }
	}
	List<DataBlob> blobs = new ArrayList<DataBlob>(jsons.size());
	for (File j : jsons) {
	    DataBlob b = null;
	    try {
		b = DataBlob.fromJSON(j);
		blobs.add(b);
	    } catch (AlreadyInDB e) {
		System.out.println("already in the database: " + j.getName());
		continue;
	    }
	}
	if (blobs.size() == 0) {
	    return;
	}
	DataBlob b0 = blobs.get(0);
	String desc = b0.e.description;
	String user = b0.e.username;
	// check that all json files share experiment info,
	//   specifically description and username
	for (DataBlob b : blobs) {
	    String currDesc = b.e.description;
	    String currUser = b.e.username;
	    if (!currDesc.equals(desc)) {
		// warn
		System.out.println("WARNING: descriptions don't all match in " + dirname);
		desc = (currDesc.length() < desc.length()) ? currDesc : desc;
		System.out.print("favoring " + desc);
		// exit
// 		System.out.println("descriptions don't match in " + dirname + ":");
// 		System.out.println("- in " + b0.c.data_source_file + ": " + desc);
// 		System.out.println("- in " + b.c.data_source_file + ": " + currDesc);
// 		System.out.println("leaving " + dirname);
//		return;
	    }
	    if (!currUser.equals(user)) {
		// warn
		System.out.println("WARNING: usernames don't all match in " + dirname);
		desc = (currUser.length() < user.length()) ? currUser : user;
		System.out.print("favoring " + desc);
		// exit
// 		System.out.println("usernames don't match in " + dirname + ":");
// 		System.out.println("- in " + b0.c.data_source_file + ": " + user);
// 		System.out.println("- in " + b.c.data_source_file + ": " + currUser);
// 		System.out.println("leaving " + dirname);
// 		return;
	    }
	}
	// create a new problem & experiment in the db
	Utils.lazilyConnectToDB();

	String prob = b0.p.problem_name;

	// find or create problem_id
	int problem_id = Utils.findByLookupOrInsert("problems",
						    "problem_name",
						    prob,
						    "problem_id");
	
	// insert experiment record with given problem_id		
	Experiment e = new Experiment(user, b0.e.datetime, desc);
	
	int eid = e.writeToDB(problem_id);	

	// push all data blobs into the db
	//   with the same experiment id
	System.out.println("writing files in " + dirname + " into db with experiment_id " + eid);
	for (DataBlob b : blobs) {
	    b.writeToDB(eid);
	}
	
    }

    static void exitWithoutWriting(String reason) throws SQLException {
	System.out.println("In DataBlob.main:");
	System.out.println(reason);
	System.out.println("Terminating without having written to the database.");
	Utils.closeDBConnection();
	System.exit(1);
    }

    static void verifyJSON(String[] filenames) {

    }

    public static void main(String[] args) 
	throws ClassNotFoundException, SQLException, IOException, JSONException {

	if (args.length == 0) {
	    exitWithoutWriting("No arguments given.");
	}
	
	Utils.lazilyConnectToDB();

	for (String arg : args) {
	    File f = new File(arg);
	    String filename = f.getName();
	    if (f.isDirectory()) {
		// System.out.println("I will process " + f + " as a directory.");
		writeDirToDB(f.getAbsolutePath());
	    } else {
		if (!filename.endsWith(".json")) {
		    // System.out.println("skipping " + filename );
		    continue;
		} else {
		    // System.out.println("I will process " + f + " as a file");
		    System.out.println("Writing " + filename + " to the database...");
		    try {
			DataBlob b = fromJSON(arg);
			b.writeToDB();
		    } catch (AlreadyInDB e) {
			System.out.println("already in database: " + arg);
		    }
		}
	    }
	}

	Utils.closeDBConnection();

    }
	
}
