import java.util.concurrent.atomic.AtomicIntegerArray;

class BetterSafeState_AIA implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    BetterSafeState_AIA(byte[] v){ 
        this(v, (byte) 127);
    }

    BetterSafeState_AIA(byte[] v, byte m){ 
        value = new AtomicIntegerArray(v.length);
        for (int i = 0; i < v.length; i++)
            value.set(i, v[i]);
        maxval = m; 
    }

    public int size(){
        return value.length();
    }

    public byte[] current(){
        byte[] copy = new byte[value.length()];
        for (int i = 0; i < value.length(); i++)
            copy[i] = (byte) value.get(i);
        
        return copy; 
    }

    public boolean swap(int i, int j) {
        int i_val = value.get(i);
        int j_val = value.get(j);
        if (i_val <= 0 || j_val >= maxval) {
            return false;
        }
        value.getAndDecrement(i);
        value.getAndIncrement(j);
        return true;
    }
}
