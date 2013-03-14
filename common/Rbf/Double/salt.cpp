#include <math.h>

#include <SALT.h>

using namespace SALT;

template<class T>
class MemVector : public VectorBase<T>, public VExpr< MemVector<T> >
{
public:
    MemVector(int length, T* data) : VectorBase<T>(length)
    {
        VectorBase<T>::p = data;
    }

    ~MemVector()
    {
    }

    template<class A>
    MemVector<T>& operator=(const VExpr<A>& a)
    {
        const A& ao ( a );

        assert(ao.size() == this->size());

        execute(store1(VectorBase<T>::p, a));

        return *this;
    }

    MemVector<T>& operator=(const MemVector<T>& a)
    {
        const MemVector<T>& ao ( a );

        assert(ao.size() == this->size());

        execute(store1(VectorBase<T>::p, a));

        return *this;
    }
};

template<class T>
double norm2(VExpr<T> const& x)
{
    return dot(x,x);
}

extern "C" double salt_rbf(double nu, double* __restrict__ u0, int ul, double* __restrict__ v0, int vl)
{
    int               n = ul < vl ? ul : vl;
    MemVector<double> u(n, u0);
    MemVector<double> v(n, v0);
    VExprSub<MemVector<double>, MemVector<double> > z = u - v;

    return exp(-nu*dot(z, z));
}

extern "C" double salt_rbf_abs(double nu, double* __restrict__ u0, int ul, double* __restrict__ v0, int vl)
{
    int               n = ul < vl ? ul : vl;
    MemVector<double> u(n, u0);
    MemVector<double> v(n, v0);

    return exp(-nu*norm2(u-v));
}
