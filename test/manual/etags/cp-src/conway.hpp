/* ======================================================================= */
/*                                 CONWAY.H                                */
/* ======================================================================= */

class site: public location
    {
    char x, y, alive, next_alive;
    int total_surrounding(void);
public:
    site(int xi, int yi): x(xi), y(yi), alive(0) { }
    ~site();
    char read() { return alive; }
    void set(void) { alive = 1; }
    void clear(void) { alive = 0; }
    void compute_next_state(void)
        {
        int n = total_surrounding();
        next_alive = alive;
        if (n < 2 || n > 3) next_alive = 0;
        else if (n > 2) next_alive = 1;
        }
    void step(void) { alive = next_alive; }
    };
