#ifndef __VECTOR_HEADER__
#define __VECTOR_HEADER__

#include <cmath>
#include <cstring>

#pragma warning(push)
#pragma warning(disable : 4244)


#ifndef M_PI
#define M_PI 3.141592653589793238462643383279502
#endif 

//==========[ Forward References ]=========================

template <class T> class Vec;
template <class T> class Vec3;
template <class T> class Vec4;
template <class T> class Mat3;
template <class T> class Mat4;

//==========[ class Vec3 ]=================================

template <class T>
class Vec3 {

	//---[ Private Variable Declarations ]-------

		// x, y, z
	T		n[3];

public:

	//---[ Constructors ]------------------------

	Vec3() { n[0] = 0.0; n[1] = 0.0; n[2] = 0.0; }
	Vec3( const T x, const T y, const T z )
		{ n[0] = x; n[1] = y; n[2] = z; }
	Vec3( const Vec3<T>& v )
		{ n[0] = v.n[0]; n[1] = v.n[1]; n[2] = v.n[2]; }

	//---[ Equal Operators ]---------------------

	Vec3<T>& operator=( const Vec3<T>& v )
		{ n[0] = v.n[0]; n[1] = v.n[1]; n[2] = v.n[2]; return *this; }
	Vec3<T>& operator +=( const Vec3<T>& v )
		{ n[0] += v.n[0]; n[1] += v.n[1]; n[2] += v.n[2]; return *this; }
	Vec3<T>& operator -= ( const Vec3<T>& v )
		{ n[0] -= v.n[0]; n[1] -= v.n[1]; n[2] -= v.n[2]; return *this; }
	Vec3<T>& operator *= ( const T d )
		{ n[0] *= d; n[1] *= d; n[2] *= d; return *this; }
	Vec3<T>& operator /= ( const T d )
		{ n[0] /= d; n[1] /= d; n[2] /= d; return *this; }

	//---[ Access Operators ]--------------------

	T& operator []( int i )
		{ return n[i]; }
	T operator []( int i ) const 
		{ return n[i]; }

	//---[ Arithmetic Operators ]----------------

	Vec3<T> operator-( const Vec3<T>& a ) const { return Vec3<T>(n[0]-a.n[0],n[1]-a.n[1],n[2]-a.n[2]); }
	Vec3<T> operator+( const Vec3<T>& a ) const { return Vec3<T>(a.n[0]+n[0],a.n[1]+n[1],a.n[2]+n[2]); }

	//---[ Conversion Operators ]----------------

	const T* getPointer() const { return n; }

	//---[ Length Methods ]----------------------

	double length2() const
		{ return n[0]*n[0] + n[1]*n[1] + n[2]*n[2]; }
	double length() const
		{ return sqrt( length2() ); }

	//---[ Normalization ]-----------------------

	void normalize() { 
		double len = length();
		n[0] /= len; n[1] /= len; n[2] /= len;
	}

	//---[ Zero Test ]---------------------------

	bool isZero() const { return ( (n[0]==0 && n[1]==0 && n[2]==0) ? true : false); };
	void zeroElements() { memset(n,0,sizeof(T)*3); }

	//---[ Other comparisons (MZB) ]---------------------------

	bool operator>( const Vec3<T>& a ) const { return n[0] > a.n[0] && n[1] > a.n[1] && n[2] > a.n[2]; }
	bool operator<( const Vec3<T>& a ) const { return n[0] < a.n[0] && n[1] < a.n[1] && n[2] < a.n[2]; }
	bool operator>=( const Vec3<T>& a ) const { return *this > a || *this == a; }
	bool operator<=( const Vec3<T>& a ) const { return *this < a || *this == a; }
	bool operator==( const Vec3<T>& a ) const { return n[0] == a.n[0] && n[1] == a.n[1] && n[2] == a.n[2]; }
	bool operator!=( const Vec3<T>& a ) const { return !(*this == a); }

	//---[ Friend Methods ]----------------------

	template <class U> friend Vec3<U> operator -( const Vec3<U>& v );
	template <class U> friend Vec3<U> operator *( const Vec3<U>& a, const double d );
	template <class U> friend Vec3<U> operator *( const double d, const Vec3<U>& a );
	template <class U> friend Vec3<U> operator *( const Vec3<U>& v, Mat4<U>& a );
	template <class U> friend U operator *( const Vec3<U>& a, const Vec3<U>& b );
	template <class U> friend Vec3<U> operator *( const Mat3<U>& a, const Vec3<U>& v );
	template <class U> friend Vec3<U> operator *( const Vec3<U>& v, const Mat3<U>& a );
	template <class U> friend Vec3<U> operator *( const Mat4<U>& a, const Vec3<U>& v );
	template <class U> friend Vec3<U> operator /( const Vec3<U>& a, const double d );
	template <class U> friend Vec3<U> operator ^( const Vec3<U>& a, const Vec3<U>& b );
	template <class U> friend bool operator ==( const Vec3<U>& a, const Vec3<U>& b );
	template <class U> friend bool operator !=( const Vec3<U>& a, const Vec3<U>& b );
	template <class U> friend Vec3<U> minimum( const Vec3<U>& a, const Vec3<U>& b );
	template <class U> friend Vec3<U> maximum( const Vec3<U>& a, const Vec3<U>& b );
	template <class U> friend Vec3<U> prod( const Vec3<U>& a, const Vec3<U>& b );
	template <class U> friend Vec3<U> cross( const Vec3<U>& a, const Vec3<U>& b );
};

typedef Vec3<int> Vec3i;
typedef Vec3<float> Vec3f;
typedef Vec3<double> Vec3d;

//==========[ class Vec4 ]=================================

template <class T>
class Vec4 {

	//---[ Private Variable Declarations ]-------

		// x, y, z, w
	T		n[4];

public:
	
	//---[ Constructors ]------------------------

	Vec4() { n[0] = 0.0; n[1] = 0.0; n[2] = 0.0; n[3] = 0.0; }
	Vec4( const T x, const T y, const T z, const T w )
		{ n[0] = x; n[1] = y; n[2] = z; n[3] = w; }
	Vec4( const Vec4& v )
		{ n[0] = v.n[0]; n[1] = v.n[1]; n[2] = v.n[2]; n[3] = v.n[3]; }

	//---[ Equal Operators ]---------------------

	Vec4<T>& operator =( const Vec4<T>& v )
		{ n[0] = v.n[0]; n[1] = v.n[1]; n[2] = v.n[2]; n[3] = v.n[3];
		  return *this; }
	Vec4<T>& operator +=( const Vec4<T>& v )
		{ n[0] += v.n[0]; n[1] += v.n[1]; n[2] += v.n[2]; n[3] += v.n[3];
		  return *this; }
	Vec4<T>& operator -= ( const Vec4<T>& v )
		{ n[0] -= v.n[0]; n[1] -= v.n[1]; n[2] -= v.n[2]; n[3] -= v.n[3];
		  return *this; }
	Vec4<T>& operator *= ( const T d )
		{ n[0] *= d; n[1] *= d; n[2] *= d; n[3] *= d; return *this; }
	Vec4<T>& operator /= ( const T d )
		{ n[0] /= d; n[1] /= d; n[2] /= d; n[3] /= d; return *this; }

	//---[ Access Operators ]--------------------

	T& operator []( int i )
		{ return n[i]; }
	T operator []( int i ) const 
		{ return n[i]; }

	//---[ Arithmetic Operators ]----------------

	Vec4<T> operator-( const Vec4<T>& a ) const { return Vec4<T>(n[0]-a.n[0],n[1]-a.n[1],n[2]-a.n[2],n[3]-a.n[3]); }
	//Vec4<T> operator+( const Vec4<T>& a ) const { return Vec4<T>(a.n[0]+n[0],a.n[1]+n[1],a.n[2]+n[2],a.n[3]-n[3]); }
	////BUGBUG! This is not special relativity!
	Vec4<T> operator+( const Vec4<T>& a ) const { return Vec4<T>(a.n[0]+n[0],a.n[1]+n[1],a.n[2]+n[2],a.n[3]+n[3]); }

	//---[ Length Methods ]----------------------

	double length2() const
		{ return n[0]*n[0] + n[1]*n[1] + n[2]*n[2] + n[3]*n[3]; }
	double length() const
		{ return sqrt( length2() ); }

	//---[ Zero Test ]---------------------------

	bool isZero() const { return n[0]==0&&n[1]==0&&n[2]==0&&n[3]==0; }
	void zeroElements() { memset(n,0,4*sizeof(T)); }

	//---[ Other comparisons (MZB) ]---------------------------

	bool operator>( const Vec4<T>& a ) const { return n[0] > a.n[0] && n[1] > a.n[1] && n[2] > a.n[2] && n[3] > a.n[3]; }
	bool operator<( const Vec4<T>& a ) const { return n[0] < a.n[0] && n[1] < a.n[1] && n[2] < a.n[2] && n[3] < a.n[3]; }
	bool operator==( const Vec4<T>& a ) const { return n[0] == a.n[0] && n[1] == a.n[1] && n[2] == a.n[2] && n[3] == a.n[3]; }
	bool operator!=( const Vec4<T>& a ) const { return !(*this == a); }


	//---[ Normalization ]-----------------------

	void normalize() {
		double len = length();
		n[0] /= len; n[1] /= len; n[2] /= len; n[3] /= len;
	}
	
	//---[ Friend Methods ]----------------------

	template <class U> friend Vec4<U> operator -( const Vec4<U>& v );
	template <class U> friend Vec4<U> operator *( const Vec4<U>& a, const double d );
	template <class U> friend Vec4<U> operator *( const double d, const Vec4<U>& a );
	template <class U> friend U operator *( const Vec4<U>& a, const Vec4<U>& b );
	template <class U> friend Vec4<U> operator *( const Mat4<U>& a, const Vec4<U>& v );
	template <class U> friend Vec4<U> operator *( const Vec4<U>& v, const Mat4<U>& a );
	template <class U> friend Vec4<U> operator /( const Vec4<U>& a, const double d );
	template <class U> friend bool operator ==( const Vec4<U>& a, const Vec4<U>& b );
	template <class U> friend bool operator !=( const Vec4<U>& a, const Vec4<U>& b );
	template <class U> friend Vec4<U> minimum( const Vec4<U>& a, const Vec4<U>& b );
	template <class U> friend Vec4<U> maximum( const Vec4<U>& a, const Vec4<U>& b );
	template <class U> friend Vec4<U> prod( const Vec4<U>& a, const Vec4<U>& b );
};

typedef Vec4<int> Vec4i;
typedef Vec4<float> Vec4f;
typedef Vec4<double> Vec4d;


//==========[ Inline Method Definitions (Vectors) ]========

template <class T>
inline T operator *( const Vec3<T>& a, const Vec4<T>& b ) {
	return a[0] * b[0] + a[1] * b[1] + a[2] * b[2] + b[3];
}

template <class T>
inline T operator *( const Vec4<T>& b, const Vec3<T>& a ) {
	return a[0] * b[0] + a[1] * b[1] + a[2] * b[2] + b[3];
}

template <class T>
inline Vec3<T> operator -(const Vec3<T>& v) {
	return Vec3<T>( -v.n[0], -v.n[1], -v.n[2] );
}

template <class T>
inline Vec3<T> operator *(const Vec3<T>& a, const double d ) {
	return Vec3<T>( a.n[0] * d, a.n[1] * d, a.n[2] * d );
}

template <class T>
inline Vec3<T> operator *(const double d, const Vec3<T>& a) {
	return a * d;
}

template <class T>
inline Vec3<T> operator *(const Mat4<T>& a, const Vec3<T>& v){
	return Vec3<T>( a.n[0]*v.n[0]+a.n[1]*v.n[1]+a.n[2]*v.n[2]+a.n[3],
					a.n[4]*v.n[0]+a.n[5]*v.n[1]+a.n[6]*v.n[2]+a.n[7],
					a.n[8]*v.n[0]+a.n[9]*v.n[1]+a.n[10]*v.n[2]+a.n[11] );
}

template <class T>
inline Vec3<T> operator *(const Vec3<T>& v, Mat4<T>& a) {
	return a.transpose() * v;
}

template <class T>
inline T operator *(const Vec3<T>& a, const Vec3<T>& b){
	return a.n[0]*b.n[0] + a.n[1]*b.n[1] + a.n[2]*b.n[2];
}

template <class T>
inline Vec3<T> operator *( const Mat3<T>& a, const Vec3<T>& v ) {
	return Vec3<T>( a.n[0]*v.n[0]+a.n[1]*v.n[1]+a.n[2]*v.n[2],
					a.n[3]*v.n[0]+a.n[4]*v.n[1]+a.n[5]*v.n[2],
					a.n[6]*v.n[0]+a.n[7]*v.n[1]+a.n[8]*v.n[2] );
}

template <class T>
inline Vec3<T> operator *( const Vec3<T>& v, const Mat3<T>& a ) {
	return a.transpose() * v;
}

template <class T>
inline Vec3<T> operator /(const Vec3<T>& a, const double d){
	return Vec3<T>( a.n[0] / d, a.n[1] / d, a.n[2] / d );
}

template <class T>
inline Vec3<T> operator ^(const Vec3<T>& a, const Vec3<T>& b) {
	return Vec3<T>( a.n[1]*b.n[2] - a.n[2]*b.n[1],
					a.n[2]*b.n[0] - a.n[0]*b.n[2],
					a.n[0]*b.n[1] - a.n[1]*b.n[0] );
}

template <class T>
inline bool operator ==(const Vec3<T>& a, const Vec3<T>& b) {
	return a.n[0]==b.n[0] && a.n[1] == b.n[1] && a.n[2] == b.n[2];
}

template <class T>
inline bool operator !=(const Vec3<T>& a, const Vec3<T>& b) {
	return !( a == b );
}


template <class T>
inline void swap( Vec3<T>& a, Vec3<T>& b ) {
	Vec3<T> t( a );
	a = b;
	b = t;
}

template <class T>
inline Vec3<T> minimum( const Vec3<T>& a, const Vec3<T>& b ) {
	return Vec3<T>( minimum(a.n[0],b.n[0]), minimum(a.n[1],b.n[1]), minimum(a.n[2],b.n[2]) );
}

template <class T>
inline Vec3<T> maximum(const Vec3<T>& a, const Vec3<T>& b) {
	return Vec3<T>( maximum(a.n[0],b.n[0]), maximum(a.n[1],b.n[1]), maximum(a.n[2],b.n[2]) );
}

template <class T>
inline Vec3<T> prod(const Vec3<T>& a, const Vec3<T>& b ) {
	return Vec3<T>( a.n[0]*b.n[0], a.n[1]*b.n[1], a.n[2]*b.n[2] );
}

template <class T>
inline Vec3<T> cross(const Vec3<T>& a, const Vec3<T>& b ) {
	return Vec3<T>( a.n[1]*b.n[2] - a.n[2]*b.n[1], 
		a.n[2]*b.n[0] - a.n[0]*b.n[2], 
		a.n[0]*b.n[1] - a.n[1]*b.n[0] );
}

template <class T>
inline Vec4<T> operator -( const Vec4<T>& v ) {
	return Vec4<T>( -v.n[0], -v.n[1], -v.n[2], -v.n[3] );
}

template <class T>
inline Vec4<T> operator *(const Vec4<T>& a, const double d ) {
	return Vec4<T>( a.n[0] * d, a.n[1] * d, a.n[2] * d, a.n[3] * d );
}

template <class T>
inline Vec4<T> operator *(const double d, const Vec4<T>& a) {
	return a * d;
}

template <class T>
inline T operator *(const Vec4<T>& a, const Vec4<T>& b) {
	return a.n[0]*b.n[0] + a.n[1]*b.n[1] + a.n[2]*b.n[2] + a.n[3]*b.n[3];
}

template <class T>
inline Vec4<T> operator *(const Mat4<T>& a, const Vec4<T>& v) {
	return Vec4<T>( a.n[0]*v.n[0]+a.n[1]*v.n[1]+a.n[2]*v.n[2]+a.n[3]*v.n[3],
					a.n[4]*v.n[0]+a.n[5]*v.n[1]+a.n[6]*v.n[2]+a.n[7]*v.n[3],
					a.n[8]*v.n[0]+a.n[9]*v.n[1]+a.n[10]*v.n[2]+a.n[11]*v.n[3],
					a.n[12]*v.n[0]+a.n[13]*v.n[1]+a.n[14]*v.n[2]+a.n[15]*v.n[3]);
}

template <class T>
inline Vec4<T> operator *( const Vec4<T>& v, Mat4<T>& a ){
	return a.transpose() * v;
}

template <class T>
inline Vec4<T> operator /(const Vec4<T>& a, const double d) {
	return Vec4<T>( a.n[0] / d, a.n[1] / d, a.n[2] / d, a.n[3] / d );
}

template <class T>
inline bool operator ==(const Vec4<T>& a, const Vec4<T>& b) {
	return a.n[0] == b.n[0] && a.n[1] == b.n[1] && a.n[2] == b.n[2] 
	    && a.n[3] == b.n[3];
}

template <class T>
inline bool operator !=(const Vec4<T>& a, const Vec4<T>& b) {
	return !( a == b );
}


template <class T>
inline void swap( Vec4<T>& a, Vec4<T>& b ) {
	Vec4<T> t( a );
	a = b;
	b = t;
}

template <class T>
inline Vec4<T> minimum( const Vec4<T>& a, const Vec4<T>& b ) {
	return Vec4<T>( minimum(a.n[0],b.n[0]), minimum(a.n[1],b.n[1]), minimum(a.n[2],b.n[2]),
	             minimum(a.n[3],b.n[3]) );
}

template <class T>
inline Vec4<T> maximum(const Vec4<T>& a, const Vec4<T>& b) {
	return Vec4<T>( maximum(a.n[0],b.n[0]), maximum(a.n[1],b.n[1]), maximum(a.n[2],b.n[2]),
	             maximum(a.n[3],b.n[3]) );
}

template <class T>
inline Vec4<T> prod(const Vec4<T>& a, const Vec4<T>& b ) {
	return Vec4<T>( a.n[0]*b.n[0], a.n[1]*b.n[1], a.n[2]*b.n[2], a.n[3]*b.n[3] );
}

#pragma warning(pop)

#endif

