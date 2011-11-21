import java.sql.SQLException;

public class Context {

    boolean is_manticore = false;
	
    Integer context_id = null;
	
    String compiler_src_url = null;
    Integer compiler_svn = null;
    String script_url = null;
    Integer script_svn = null;
    Boolean seq_compilation = null;
    Integer max_leaf_size = null;
    Integer seq_cutoff = null;

    final String language;
    final String compiler;
    final String version;

    final String bench_url;
    final int bench_svn;
    final String input;
    final String username;
    final String machine;
    final String datetime;

    final String data_source_file;       
	
    public Context(
		   String language,
		   String compiler,
		   String version,
		   String bench_url,
		   int bench_svn,
		   String input,
		   String username,
		   String machine,
		   String datetime,
		   String data_source_file) {
	this.language = language;
	this.compiler = compiler;
	this.version = version;
	this.bench_url = bench_url;
	this.bench_svn = bench_svn;
	this.input = input;
	this.username = username;
	this.machine = machine;
	this.datetime = datetime;
	this.data_source_file = data_source_file;
    }

    void setManticoreInfo(String compiler_src_url,
			  int compiler_svn,
			  String script_url,
			  int script_svn,
			  boolean seq_compilation,
			  Integer max_leaf_size,
			  Integer seq_cutoff) {
	this.compiler_src_url = compiler_src_url;
	this.compiler_svn = compiler_svn;
	this.script_url = script_url;
	this.script_svn = script_svn;
	this.seq_compilation = seq_compilation;
	this.max_leaf_size = max_leaf_size;
	this.seq_cutoff = seq_cutoff;
	this.is_manticore = true;
    }
	
    private void ins(String field, String value)
	throws ClassNotFoundException, SQLException {
	Utils.updateByKey("contexts", 
			  "context_id", 
			  String.valueOf(this.context_id), 
			  field, 
			  value);
    }
	
    private void ins(String field, int value) 
	throws ClassNotFoundException, SQLException {
	ins(field, String.valueOf(value));
    }
	
    int writeToDB(int experiment_id) throws ClassNotFoundException, SQLException {

	// SIDE EFFECT: stashes new key in this.context_id
	// SIDE EFFECT: returns new key
	String[] fieldNames = {"experiment_id",
			       "language",
			       "compiler",
			       "version",
			       "bench_url",
			       "bench_svn",
			       "input",
			       "username",
			       "machine",
			       "datetime",
			       "data_source_file"};

	String[] values = {String.valueOf(experiment_id),
			   this.language,
			   this.compiler,
			   this.version,
			   this.bench_url,
			   String.valueOf(this.bench_svn),
			   this.input,
			   this.username,
			   this.machine,
			   this.datetime,
                           this.data_source_file};

	int k = Utils.insertAndReturnNewKey("contexts",
					    fieldNames,
					    values,
					    "context_id");
	this.context_id = k;
		
	// add manticore fields if relevant
	if (this.is_manticore) {
	    ins("compiler_src_url", this.compiler_src_url);
	    ins("compiler_svn", this.compiler_svn);
	    ins("script_url", this.script_url);
	    ins("script_svn", String.valueOf(this.script_svn));
	    ins("seq_compilation", String.valueOf(this.seq_compilation));
	    if (max_leaf_size != null)
		ins("max_leaf_size", this.max_leaf_size);
	    if (seq_cutoff != null)
		ins("seq_cutoff", this.seq_cutoff);			
	}
		
	return k;				
    }	
}
