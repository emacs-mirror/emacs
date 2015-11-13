/* ======================================================================= */
/*                                 CLHEIR.H                                */
/* ======================================================================= */

// CLASS HEIRARCHY
// Locations or Agents are both of type generic_object. Generic_objects may
// have states, and are responsible for updating their states appropriately
// when their step() functions are executed.

extern void init_registry(void);
extern void step_everybody(void);

class generic_object
    {
    int where_in_registry;
public:
    generic_object();  // enter generic_object into ObjectRegistry
    // We never copy generic_objects, so we don't need a copy constructor.
    ~generic_object(void);  // delete from ObjectRegistry
    // Simulation steps, accommodate different kinds of time
    virtual void compute_next_state(void) { }
    virtual void step(void) { }
    };

// =======================================================================

// Locations can be regular (like a field of squares or hexagons) or
// irregular. Regular locations have 2-D or 3-D positions represented
// by integers. Locations are never copied; no need for copy constructors.

const int max_num_directions = 6; // handles both cubes and hexagons

class location: public generic_object
    {
    // Any kind of location needs a physical position, but for a regular
    // location, this may be implicit, and for an irregular location, it
    // should be custom-defined.

    // Any kind of location needs a private list of neighbors, but for a
    // regular location, this may be implicit.

public:
    location() { }
    ~location();
    };

class irregular_location: public location
    {
    double x, y, z;
public:
    irregular_location(double xi, double yi, double zi)
        { x = xi; y = yi; z = zi; }
    ~irregular_location();
    };

class discrete_location: public location
    {
    int x, y, z;
    class location *neighbors[max_num_directions];
    void clear_neighbors(void);
public:
    discrete_location(int xi, int yi, int zi):
        x(xi), y(yi), z(zi)
            { clear_neighbors(); }
    ~discrete_location(void);
    void assign_neighbor(int direction, location *x)
        { neighbors[direction] = x; }
    };

// =======================================================================

// Agents are generic_objects with locations, who can move. Examples in
// Pacman would be the protagonist, the monsters, and the monsters' ghosts.

class agent: public generic_object
    {
    location *where;
public:
    agent();
    ~agent();
    void move(int);
    };
