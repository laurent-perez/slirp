#ifndef __VEC_HH__
#define __VEC_HH__

class Vec
{
  public:

	Vec(double X, double Y) 	: _x(X), _y(Y)		{}
	Vec()				: _x(0), _y(0)		{}
	Vec(Vec& v)			: _x(v._x), _y(v._y)	{}
	double x()			{ return _x; }
	double y()			{ return _y; }
	double	dot(Vec& v2)		{ return _x*v2._x + _y*v2._y; }

  private:

	double _x;
	double _y;
};

#endif
