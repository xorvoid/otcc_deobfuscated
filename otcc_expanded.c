#include <stdio.h>
#include <string.h>

int d,z,C,h,P,K,ac,q,G,v,Q,R,D,L,W,M;

E(e)
{
  *(char*) D++ = e;
}

o()
{
  if (L) {
    h=*(char*) L++;
    if( h == 2){
      L=0;
      h=W;
    }
  }
  else h=fgetc(Q);
}


X() {
  return isalnum(h) | h == 95;
}

Y (){
  if( h == 92){
    o();
    if( h == 110)h=10;
  }
}

ad(){
  int e,j,m;
  while( isspace(h)|h == 35){
    if( h == 35){
      o ();
      ad();
      if( d == 536){
        ad();
        E(32);
        *(int*) d=1;
        *(int*)(d +4)=D;
      }
      while( h!=10){
        E(h);
        o ();
      }
      E(h);
      E(2);
    }
    o ();
  }
  C=0;
  d=h;
  if( X ()){
    E(32);
    M=D;
    while( X ()){
      E(h);
      o ();
    }
    if( isdigit(d)){
      z=strtol(M,0,0);
      d=2;
    }
    else{
      *(char*) D=32;
      d=strstr(R,M-1)-R;
      *(char*) D=0;
      d=d*8+256;
      if( d>536){
        d=P+d;
        if( *(int*) d == 1){
          L=*(int*)(d +4);
          W=h;
          o ();
          ad();
        }
      }
    }
  }
  else{
    o ();
    if( d == 39){
      d=2;
      Y ();
      z=h;
      o ();
      o ();
    }
    else if( d == 47&h == 42){
      o ();
      while( h){
        while( h!=42)o ();
        o ();
        if( h == 47)h=0;
      }
      o ();
      ad();
    }
    else{
      e="++#m--%am*@R<^1c/@%[_[H3c%@%[_[H3c+@.B#d-@%:_^BKd<<Z/03e>>`/03e<=0f>=/f<@.f>@1f==&g!='g&&k||#l&@.BCh^@.BSi|@.B+j~@/%Yd!@&d*@b";
      while( j=*(char*) e++){
        m= *(char*) e++;
        z=0;
        while((C=*(char*) e++-98)<0)z=z*64+C+64;
        if( j == d&(m == h|m == 64)){
          if( m == h){
            o ();
            d=1;
          }
          break;
        }
      }
    }
  }
}

ae( g){
  while( g&&g!=-1){
    *(char*) q++=g;
    g=g>>8;
  }
}

A(e){
  int g;
  while( e){
    g=*(int*) e;
    *(int*) e=q-e-4;
    e=g;
  }
}

s(g,e){
  ae( g);
  *(int*) q=e;
  e=q;
  q=q +4;
  return e;
}

H(e){
  s(184,e);
}

B(e){
  return s(233,e);
}

S(j,e){
  ae( 1032325);
  return s(132+j,e);
}

Z(e){
  ae( 49465);
  H(0);
  ae( 15);
  ae( e+144);
  ae( 192);
}

N(j,e){
  ae( j+131);
  s((e<512)<<7|5,e);
}

T (j){
  int g,e,m,aa;
  g=1;
  if( d == 34){
    H(v);
    while( h!=34){
      Y ();
      *(char*) v++=h;
      o ();
    }
    *(char*) v=0;
    v=v +4&-4;
    o ();
    ad();
  }
  else{
    aa=C;
    m= z;
    e=d;
    ad();
    if( e == 2){
      H(m);
    }
    else if( aa == 2){
      T(0);
      s(185,0);
      if( e == 33)Z(m);
      else ae( m);
    }
    else if( e == 40){
      w ();
      ad();
    }
    else if( e == 42){
      ad();
      e=d;
      ad();
      ad();
      if( d == 42){
        ad();
        ad();
        ad();
        ad();
        e=0;
      }
      ad();
      T(0);
      if( d == 61){
        ad();
        ae( 80);
        w ();
        ae( 89);
        ae( 392+(e == 256));
      }
      else if( e){
        if( e == 256)ae( 139);
        else ae( 48655);
        q++;
      }
    }
    else if( e == 38){
      N(10,*(int*) d);
      ad();
    }
    else{
      g=*(int*) e;
      if(!g)g=dlsym(0,M);
      if( d == 61&j){
        ad();
        w ();
        N(6,g);
      }
      else if( d!= 40){
        N(8,g);
        if( C == 11){
          N(0,g);
          ae( z);
          ad();
        }
      }
    }
  }
  if( d == 40){
    if( g == 1)ae( 80);
    m= s(60545,0);
    ad();
    j=0;
    while( d!= 41){
      w ();
      s(2393225,j);
      if( d == 44)ad();
      j=j +4;
    }
    *(int*) m= j;
    ad();
    if(!g){
      e=e +4;
      *(int*) e=s(232,*(int*) e);
    }
    else if( g == 1){
      s(2397439,j);
      j=j +4;
    }
    else{
      s(232,g-q-5);
    }
    if( j)s(50305,j);
  }
}

O (j){
  int e,g,m;
  if( j--== 1)T(1);
  else{
    O (j);
    m= 0;
    while( j == C){
      g=d;
      e=z;
      ad();
      if( j>8){
        m= S(e,m);
        O (j);
      }
      else{
        ae( 80);
        O (j);
        ae( 89);
        if( j == 4|j == 5){
          Z(e);
        }
        else{
          ae( e);
          if( g == 37)ae( 146);
        }
      }
    }
    if( m&&j>8){
      m= S(e,m);
      H(e^1);
      B(5);
      A(m);
      H(e);
    }
  }
}

w (){
  O(11);
}

U (){
  w ();
  return S(0,0);
}

I (j){
  int m,g,e;
  if( d == 288){
    ad();
    ad();
    m= U ();
    ad();
    I (j);
    if( d == 312){
      ad();
      g=B(0);
      A(m);
      I (j);
      A(g);
    }
    else{
      A(m);
    }
  }
  else if( d == 352|d == 504){
    e=d;
    ad();
    ad();
    if( e == 352){
      g=q;
      m= U ();
    }
    else{
      if( d!= 59)w ();
      ad();
      g=q;
      m= 0;
      if( d!= 59)m= U ();
      ad();
      if( d!= 41){
        e=B(0);
        w ();
        B(g-q-5);
        A(e);
        g=e +4;
      }
    }
    ad();
    I(&m);
    B(g-q-5);
    A(m);
  }
  else if( d == 123){
    ad();
    ab(1);
    while( d!= 125)I (j);
    ad();
  }
  else{
    if( d == 448){
      ad();
      if( d!= 59)w ();
      K=B(K);
    }
    else if( d == 400){
      ad();
      *(int*) j=B(*(int*) j);
    }
    else if( d!= 59)w ();
    ad();
  }
}

ab (j){
  int m;
  while( d == 256|d!=-1&!j){
    if( d == 256){
      ad();
      while( d!= 59){
        if( j){
          G=G +4;
          *(int*) d=-G;
        }
        else{
          *(int*) d=v;
          v=v +4;
        }
        ad();
        if( d == 44)ad();
      }
      ad();
    }
    else{
      A(*(int*)(d +4));
      *(int*) d=q;
      ad();
      ad();
      m= 8;
      while( d!= 41){
        *(int*) d=m;
        m= m +4;
        ad();
        if( d == 44)ad();
      }
      ad();
      K=G=0;
      ae( 15042901);
      m= s(60545,0);
      I(0);
      A(K);
      ae( 50121);
      *(int*) m= G;
    }
  }
}

main(g,e){
  Q=__stdinp;
  if( g-->1){
    e=e +4;
    Q=fopen(*(int*) e,"r");
  }
  D=strcpy(R =calloc(1,99999)," int if else while break return for define main ")+48;
  v =calloc(1,99999);
  q=ac =calloc(1,99999);
  P =calloc(1,99999);
  o ();
  ad();
  ab(0);
  return(*(int(*)())*(int*)(P+592))(g,e);
}
