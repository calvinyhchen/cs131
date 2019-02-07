import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSet implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    GetNSet(byte[] v) { 
        int [] intval = new int[v.length];
        for(int i=0; i<v.length; i++){
            intval[i] = v[i];
        }
        value = new AtomicIntegerArray( intval ); 
        maxval = 127; 
    }

    GetNSet(byte[] v, byte m) { 
        int [] intval = new int[v.length];
        for(int i=0; i<v.length; i++){
            intval[i] = v[i];
        }
        value = new AtomicIntegerArray( intval ); 
        maxval = m; 
    }

    public int size() { return value.length(); }

    public byte[] current() { 
        byte [] byteval = new byte[value.length()];
        for(int i=0; i<value.length(); i++){
            byteval[i] = (byte) value.get(i);
        } 
        return byteval;
    }

    public boolean swap(int i, int j) {
    	if (value.get(i) <= 0 || value.get(j) >= maxval) {
    	    return false;
    	}
        value.set(i, value.get(i)-1);  //value[i]--;
    	value.set(j, value.get(j)+1);  //value[j]++;
    	return true;
    }
}
