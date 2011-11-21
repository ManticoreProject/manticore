import java.sql.SQLException;

public class GC {

    static String[] fieldNamesWithoutTime = {
	    "run_id",
	    "processor",
	    "minor_n_collections",
	    "minor_alloc_bytes",
	    "minor_collected_bytes",
	    "minor_copied_bytes",
	    "minor_time_coll_sec",
	    "major_n_collections",
	    "major_alloc_bytes",
	    "major_collected_bytes",
	    "major_copied_bytes",
	    "major_time_coll_sec",
	    "global_n_collections",
	    "global_alloc_bytes",
	    "global_collected_bytes",
	    "global_copied_bytes",
	    "global_time_coll_sec",
	    "n_promotions",
	    "prom_bytes",
	    "mean_prom_time_sec"};

    static String[] fieldNamesWithTime = {
	    "run_id",
	    "processor",
	    "minor_n_collections",
	    "minor_alloc_bytes",
	    "minor_collected_bytes",
	    "minor_copied_bytes",
	    "minor_time_coll_sec",
	    "major_n_collections",
	    "major_alloc_bytes",
	    "major_collected_bytes",
	    "major_copied_bytes",
	    "major_time_coll_sec",
	    "global_n_collections",
	    "global_alloc_bytes",
	    "global_collected_bytes",
	    "global_copied_bytes",
	    "global_time_coll_sec",
	    "n_promotions",
	    "prom_bytes",
	    "mean_prom_time_sec",
	    "elapsed_time_sec"};

    String[] valuesWithoutTime() {
	String[] values = {
	    String.valueOf(run_id),
	    String.valueOf(processor),
	    String.valueOf(minor_n_collections),
	    String.valueOf(minor_alloc_bytes),
	    String.valueOf(minor_collected_bytes),
	    String.valueOf(minor_copied_bytes),
	    String.valueOf(minor_time_coll_sec),
	    String.valueOf(major_n_collections),
	    String.valueOf(major_alloc_bytes),
	    String.valueOf(major_collected_bytes),
	    String.valueOf(major_copied_bytes),
	    String.valueOf(major_time_coll_sec),
	    String.valueOf(global_n_collections),
	    String.valueOf(global_alloc_bytes),
	    String.valueOf(global_collected_bytes),
	    String.valueOf(global_copied_bytes),
	    String.valueOf(global_time_coll_sec),
	    String.valueOf(n_promotions),
	    String.valueOf(prom_bytes),
	    String.valueOf(mean_prom_time_sec)};
	return values;
    }

    String[] valuesWithTime() {
	String[] values = {
	    String.valueOf(run_id),
	    String.valueOf(processor),
	    String.valueOf(minor_n_collections),
	    String.valueOf(minor_alloc_bytes),
	    String.valueOf(minor_collected_bytes),
	    String.valueOf(minor_copied_bytes),
	    String.valueOf(minor_time_coll_sec),
	    String.valueOf(major_n_collections),
	    String.valueOf(major_alloc_bytes),
	    String.valueOf(major_collected_bytes),
	    String.valueOf(major_copied_bytes),
	    String.valueOf(major_time_coll_sec),
	    String.valueOf(global_n_collections),
	    String.valueOf(global_alloc_bytes),
	    String.valueOf(global_collected_bytes),
	    String.valueOf(global_copied_bytes),
	    String.valueOf(global_time_coll_sec),
	    String.valueOf(n_promotions),
	    String.valueOf(prom_bytes),
	    String.valueOf(mean_prom_time_sec),
	    String.valueOf(elapsed_time_sec)};
	return values;
    }
	
    Integer gc_id = null;
    Integer run_id = null;
    int processor;
    int minor_n_collections;
    long minor_alloc_bytes;
    long minor_collected_bytes;
    long minor_copied_bytes;
    double minor_time_coll_sec;
    int major_n_collections;
    long major_alloc_bytes;
    long major_collected_bytes;
    long major_copied_bytes;
    double major_time_coll_sec;
    int global_n_collections;
    long global_alloc_bytes;
    long global_collected_bytes;
    long global_copied_bytes;
    double global_time_coll_sec;
    int n_promotions;
    long prom_bytes;
    double mean_prom_time_sec;
    Double elapsed_time_sec; // might be null

    public GC(int processor,
	      int minor_n_collections,
	      long minor_alloc_bytes,
	      long minor_collected_bytes,
	      long minor_copied_bytes,
	      double minor_time_coll_sec,
	      int major_n_collections,
	      long major_alloc_bytes,
	      long major_collected_bytes,
	      long major_copied_bytes,
	      double major_time_coll_sec,
	      int global_n_collections,
	      long global_alloc_bytes,
	      long global_collected_bytes,
	      long global_copied_bytes,
	      double global_time_coll_sec,
	      int n_promotions,
	      long prom_bytes,
	      double mean_prom_time_sec,
	      Double elapsed_time_sec) {
	this.processor = processor;
	this.minor_n_collections = minor_n_collections;
	this.minor_alloc_bytes = minor_alloc_bytes;
	this.minor_collected_bytes = minor_collected_bytes;
	this.minor_copied_bytes = minor_copied_bytes;
	this.minor_time_coll_sec = minor_time_coll_sec;
	this.major_n_collections = major_n_collections;
	this.major_alloc_bytes = major_alloc_bytes;
	this.major_collected_bytes = major_collected_bytes;
	this.major_copied_bytes = major_copied_bytes;
	this.major_time_coll_sec = major_time_coll_sec;
	this.global_n_collections = global_n_collections;
	this.global_alloc_bytes = global_alloc_bytes;
	this.global_collected_bytes = global_collected_bytes;
	this.global_copied_bytes = global_copied_bytes;
	this.global_time_coll_sec = global_time_coll_sec;
	this.n_promotions = n_promotions;
	this.prom_bytes = prom_bytes;
	this.mean_prom_time_sec = mean_prom_time_sec;
	this.elapsed_time_sec = elapsed_time_sec;
    }	      

    int writeToDB(int run_id) throws ClassNotFoundException, SQLException {
	// SIDE EFFECT: stashes new key in this.gc_id
	// SIDE EFFECT: returns new key
	// SIDE EFFECT: stashes run_id in this.run_id

	this.run_id = run_id;	

	String[] fieldNames = {};
	String[] values = {};

	if (this.elapsed_time_sec == null) {
	    fieldNames = fieldNamesWithoutTime;
	    values = this.valuesWithoutTime();
	} else {
	    fieldNames = fieldNamesWithTime;
	    values = this.valuesWithTime();
	}

	int k = Utils.insertAndReturnNewKey("gc", fieldNames, values, "gc_id");
	this.gc_id = k;
	return k;	
    }
    
}

