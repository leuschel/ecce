/* 
 * ProTcl - A Prolog Interface to the Tcl/Tk toolkit and to the
 *		Xlib library
 *
 *
 *	Copyright (c) 1995 Micha Meier
 *
 *******************************************************************/

#ifndef lint
static char     *SccsId   = "@(#)xlib.c	1.9        96/03/02";
#endif

#ifdef ECLIPSE
#include "tcl.h"
#include "tk.h"
#include "external.h"

#define Check_Xlib_Id(v, t, tkwin, gc)			\
	Error_If_Ref(t);				\
	if (IsStructure(t) && v.ptr->val.did == d_protcl_xlib_id)\
	{						\
		Check_Integer(v.ptr[1].tag)		\
		tkwin = (Tk_Window) v.ptr[1].val.nint;	\
		Check_Integer(v.ptr[2].tag)		\
		gc = (GC) v.ptr[2].val.nint;		\
	}						\
	else						\
	{						\
		Bip_Error(TYPE_ERROR);			\
	}

extern Tcl_Interp	*protcl_interp;
extern Tk_Window	protcl_mainWindow;
extern word32		d_protcl_xlib_id;

/*
 * xlib_init(+Path, -XlibID)
 *	Returns the XlibID of an existing Tk window.
 *	The XlibID is a structure xlib_id(Tk_Window, GC)
 */
p_xlib_init(vp, tp, vi, ti)
value	vp, vi;
type	tp, ti;
{
    Tk_Window			tkwin;
    GC				gc;
    XSetWindowAttributes	attr;
    char			*name;
    pword			*p;

    if (protcl_interp == 0) {
	Fail;
    }
    Get_Name(vp, tp, name)
    tkwin = Tk_NameToWindow(protcl_interp, name, protcl_mainWindow);
    if (tkwin == NULL) {
        Fail;
    }
    if (IsStructure(ti) && vi.ptr->val.did == d_protcl_xlib_id &&
		IsInteger(vi.ptr[2].tag))
	gc = (GC) vi.ptr[2].val.nint;
    else
	gc = XCreateGC(Tk_Display(tkwin), Tk_WindowId(tkwin), 0, 0);
    attr.backing_store = WhenMapped;
    Tk_ChangeWindowAttributes(tkwin, CWBackingStore, &attr);

    p = TG;
    TG += 3;
    Check_Gc;
    p[0].val.did = d_protcl_xlib_id;
    p[0].tag.all = TDICT;
    p[1].val.nint = (long) tkwin;
    p[1].tag.all = TINT;
    p[2].val.nint = (long) gc;
    p[2].tag.all = TINT;
    Return_Unify_Structure(vi, ti, p)
}

/*
 * xlib_id(+XlibID, -Display, -Window, -GC)
 *	Returns the Xlib items of a given Tk window
 */
p_xlib_id(vi, ti, vd, td, vw, tw, vg, tg)
value	vi, vd, vw, vg;
type	ti, td, tw, tg;
{
    Tk_Window	tkwin;
    GC		gc;
    Prepare_Requests;

    Check_Xlib_Id(vi, ti, tkwin, gc)
    Request_Unify_Integer(vd, td, (long) Tk_Display(tkwin))
    Request_Unify_Integer(vw, tw, (long) Tk_WindowId(tkwin))
    Request_Unify_Integer(vg, tg, (long) gc)
    Succeed;
}

/*
 * xlib_tkwin(+XlibID, -TkWindow)
 *	Returns the Tk id of a given Tk window
 */
p_xlib_tkwin(vi, ti, vw, tw)
value	vi, vw;
type	ti, tw;
{
    Tk_Window	tkwin;
    GC		gc;

    Check_Xlib_Id(vi, ti, tkwin, gc)
    Return_Unify_Integer(vw, tw, (long) tkwin)
}

/* Dummy change procedure for the image function */
void
chproc(cd, x, y, w, h, iw, ih)
ClientData cd;
int x;
int y;
int w;
int h;
int iw;
int ih;
{
}

/*
 * xlib_pixmap(+XlibID, +TkPixmap, -XlibPixmap)
 */
p_xlib_pixmap(vi, ti, vn, tn, vp, tp)
value           vi, vn, vp;
type            ti, tn, tp;
{
    char        *name;
    Pixmap      pixmap;
    Tk_Window   tkwin;
    GC          gc;
    Tk_Image    image;
    ClientData  cd;
    int         width;
    int         height;

    Check_Xlib_Id(vi, ti, tkwin, gc)
    Get_Name(vn, tn, name);
    image = Tk_GetImage(protcl_interp, tkwin, name, chproc, cd);
    if (image == NULL) {
	Fail;
    }
    Tk_SizeOfImage(image, &width, &height);
    pixmap = Tk_GetPixmap(Tk_Display(tkwin), Tk_WindowId(tkwin),
        width, height, Tk_Depth(tkwin));
    Tk_RedrawImage(image, 0, 0, width, height, pixmap, 0, 0);
    Tk_FreeImage(image);
    Return_Unify_Integer(vp, tp, pixmap);
}

/*
 * xlib_color(+XlibID, +TkColor, -XlibColor)
 */
p_xlib_color(vi, ti, vc, tc, vp, tp)
value		vi, vc, vp;
type		ti, tc, tp;
{
    char	*color;
    Tk_Window	tkwin;
    Drawable	window;
    GC		gc;
    XColor	*xcolor;

    Check_Xlib_Id(vi, ti, tkwin, gc)
    Get_Name(vc, tc, color);
    xcolor = Tk_GetColor(protcl_interp, tkwin, Tk_GetUid(color));
    if (xcolor == (XColor *) 0) {
    	Fail
    }
    Return_Unify_Integer(vp, tp, xcolor->pixel)
}

/*
 * xlib_font(+XlibID, +Font)
 */
p_xlib_font(vi, ti, vf, tf)
value		vi, vf;
type		ti, tf;
{
    char	*font;
    Tk_Window	tkwin;
    Drawable	window;
    GC		gc;
    XFontStruct	*xfont;

    Check_Xlib_Id(vi, ti, tkwin, gc)
    Get_Name(vf, tf, font);
    xfont = Tk_GetFontStruct(protcl_interp, tkwin, Tk_GetUid(font));
    if (xfont == (XFontStruct *) 0) {
    	Fail
    }
    XSetFont(Tk_Display(tkwin), gc, xfont->fid);
    Succeed;
}

/*
 * xlib_query_pointer(+XlibID, -X, -Y)
 */
p_xlib_query_pointer(vi, ti, vx, tx, vy, ty)
value		vi, vx, vy;
type		ti, tx, ty;
{
    Tk_Window	tkwin;
    Drawable	window;
    GC		gc;
    Window	r1, r2;
    int		rx, ry;
    int		wx, wy;
    int		mask;
    int		res;
    Prepare_Requests;

    Check_Xlib_Id(vi, ti, tkwin, gc)
    res = XQueryPointer(Tk_Display(tkwin), Tk_WindowId(tkwin), &r1, &r2,
		&rx, &ry, &wx, &wy, &mask);
    if (res == False) {
    	Fail
    }
    Request_Unify_Integer(vx, tx, wx);
    Request_Unify_Integer(vy, ty, wy);
    Return_Unify;
}

/*
 * xlib_polygon(XlibID, DataList, OffX, OffY, RX, RY, Xpose, Border, Fill)
 *	draw a polygon with a given scale, offset, and colours
 */
p_xlib_polygon(iv, it, lv, lt, xv, xt, yv, yt, rxv, rxt, ryv, ryt, xpv, xpt, 
	bv, bt, fv, ft)
value	iv, lv, xv, yv, rxv, ryv, xpv, bv, fv;
type	it, lt, xt, yt, rxt, ryt, xpt, bt, ft;
{
    Tk_Window		tkwin;
    GC			gc;
    XPoint		*p, *start;
    pword		*l, *r;
    int			n = 0;
    register int	xpose = xpv.nint;
    unsigned long	color;

    Check_List(lt)
    Check_Xlib_Id(iv, it, tkwin, gc)
    Check_Integer(xt)
    Check_Integer(yt)
    Check_Float(rxt)
    Check_Float(ryt)
    Check_Integer(xpt)
    Check_Integer(bt)
    Check_Integer(ft)
    if (IsNil(lt)) {
	Succeed_
    }
    p = start = (XPoint *) Gbl_Tg;
    Gbl_Tg++;
    Check_Gc;
    l = lv.ptr;
    for (;;)
    {
	    r = l++;
	    Dereference_(r);
	    if (xpose)
		p->y = FloatVal(ryv, ryt) * (yv.nint + r->val.nint);
	    else
		p->x = FloatVal(rxv, rxt) * (xv.nint + r->val.nint);
	    Dereference_(l);
	    r = l->val.ptr;
	    l = r + 1;
	    Dereference_(r);
	    if (xpose)
		p->x = FloatVal(rxv, rxt) * (xv.nint + r->val.nint);
	    else
		p->y = FloatVal(ryv, ryt) * (yv.nint + r->val.nint);
	    n++;
	    if (p++ >= (XPoint *) Gbl_Tg) {
		    Gbl_Tg++;
		    Check_Gc;
	    }
	    Dereference_(l);
	    if (IsNil(l->tag))
		    break;
	    l = l->val.ptr;
    }

    if (fv.nint >= 0) {
	XSetForeground(Tk_Display(tkwin), gc, fv.nint);
	XFillPolygon(Tk_Display(tkwin), Tk_WindowId(tkwin), gc, start,
	    n, Nonconvex, 0);
    }
    if (bv.nint >= 0) {
	XSetForeground(Tk_Display(tkwin), gc, bv.nint);
	XDrawLines(Tk_Display(tkwin), Tk_WindowId(tkwin), gc, start, n, 0);
    }
    Gbl_Tg = (pword *) start;
    Succeed_
}

/* P is on the line drawn through (x,y) */
#define OnLine(px, py, x, y)	(y == py && px >= x)


/*
 *	inside(X, Y, Cover, List),
 *		Succeeds if the point [X, Y] is inside the polygon
 *		with coordinates in List = [X1, Y1, X2, Y2, ..., 
 */
p_inside(vx, tx, vy, ty, vc, tc, vl, tl)
value	vx, vy, vc, vl;
type	tx, ty, tc, tl;
{
	pword	*p;
	pword	*s;
	int	x, y, x1, y1, x2, y2, xl, yl;
	int	cx1, cx2, cy1, cy2;
	int	count;
	int	c1, c2;

	x = vx.nint;
	y = vy.nint;

	p = vc.ptr;
	s = p++;
	Dereference_(s);
	cx1 = s->val.nint;

	Dereference_(p);
	p = p->val.ptr;
	s = p++;
	Dereference_(s);
	cy1 = s->val.nint;

	Dereference_(p);
	p = p->val.ptr;
	s = p++;
	Dereference_(s);
	cx2 = s->val.nint;

	Dereference_(p);
	p = p->val.ptr;
	s = p++;
	Dereference_(s);
	cy2 = s->val.nint;

	/* check if in the square */
	if (x < cx1 || y < cy1 || x > cx2 || y > cy2) {
	    Fail_
	}
	p = vl.ptr;
	s = p++;
	Dereference_(s);
	xl = x2 = s->val.nint;
	Dereference_(p);

	p = p->val.ptr;
	s = p++;
	Dereference_(s);
	yl = y2 = s->val.nint;
	Dereference_(p);
	/* make sure that P2 is not on the line */
	if (OnLine(x2, y2, x, y)) {
	    if (x == x2) {
		Succeed;
	    }
	    xl = x2 = x2 - 1;
	}

	count = 0;
	while (IsList(p->tag))
	{
	    p = p->val.ptr;
	    s = p++;
	    Dereference_(s);
	    x1 = s->val.nint;
	    Dereference_(p);

	    p = p->val.ptr;
	    s = p++;
	    Dereference_(s);
	    y1 = s->val.nint;
	    Dereference_(p);

	    if (OnLine(x1, y1, x, y)) {
		if (x == x1) {
		    Succeed;
		}
		continue;
	    }
	    if (y1 >= y && y2 <= y || y1 <= y && y2 >= y) {
		c1 = (x2 - x1)*(y - y1);
		c2 = (y2 - y1)*(x - x1);
		if (y2 > y1 && c1 > c2 || y2 < y1 && c1 < c2)
		    count++;
	    }
	    x2 = x1;
	    y2 = y1;
	}
	if (!OnLine(x1, y1, x, y) && (y2 >= y && yl <= y || y2 <= y && yl >= y))
	{
	    c1 = (xl - x2)*(y - y2);
	    c2 = (yl - y2)*(x - x2);
	    if (yl > y2 && c1 > c2 || yl < y2 && c1 < c2)
		count++;
	}
	Succeed_If(count & 1);
}

/*
 * Dummy function to enforce linking of useful Xlib functions
 */
static
_dummy()
{
    Display	*d;
    Drawable	w;
    GC		gc;
    XGCValues	xv;

    XDrawImageString(d, w, gc, 0, 0, "", 0);
    XSetBackground(d, gc, 0);
    XWarpPointer(d, 0, 0, 0, 0, 0, 0, 0, 0);
    XChangeGC(d, gc, 0, &xv);
    XClearArea(d, w, 0, 0, 0, 0, 0);
    XSetLineAttributes(d, gc, 0, 0, 0, 0);
}

#endif /* ECLIPSE */
