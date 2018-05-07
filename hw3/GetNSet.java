import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    GetNSetState(byte[] v){ 
        this(v, (byte) 127);
    }

    GetNSetState(byte[] v, byte m){ 
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
        value.set(i, i_val - 1);
        value.set(j, j_val + 1);
        return true;
    }
}
