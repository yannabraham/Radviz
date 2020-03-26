//#include "orange_api.hpp"
//#include "examplegen.hpp"
//#include "symmatrix.hpp"
//#include "pnn.hpp"
//#include "../orange/px/externs.px"
#include "RcppArmadillo.h"

using namespace Rcpp;


#define xfree(x) { if (x) free(x); }

typedef struct {double x, y; } TPoint;

inline double sqr(const double &x)
{ return x*x; }


/* Computes forces for discrete class
   INPUT:
     pts               projections of examples
     classes           example classes
     nExamples         number of examples (the length of above arrays and of Fr)
     law               0=Linear, 1=Square, 2=Gaussian
     sigma2            sigma**2 for Gaussian law
     attractG          the factor to multiply the attractive forces with
     repelG            the factor to multiply the repulsive forces with
     dynamicBalancing  if true, the forces are balanced (prior to multiplying with the above factors
                          so that the total sum of the attractive equals the sum of repulsive)
     Comments:
       Fa is used to return the forces, but should be allocated by the caller
       Fr is used as a temporary, but should be allocated by the caller if both types of forces are used
       attractG, repelG, dynamicBalancing are used only if both types of forces are used
   OUTPUT:
     Fa                forces acting on each example (memory should be allocated by the caller!)
*/


void computeForcesDiscreteGraph(TPoint *pts, TPoint *pts2, const TPoint *ptse, const int *edgesInds, const double *edgeWeights, const int *degreeVect,
                           int law, const double &sigma2, const double &attractG, const double &repelG, const bool dynamicBalancing,
                           TPoint *Fa, TPoint *Fr, const int nEdges
                          )
{
  TPoint *Fai, *Fri, *Fri2, *Fe, *ptsi, *ptsi2;
  const int *edgesIndsi;
  const double *edgeWeightsi;
  const int *degreeVecti;
  int i;

  for(Fai = Fa, Fe = Fa + (ptse-pts); Fai != Fe; Fai++)
 	  	  Fai->x = Fai->y = 0.0;

  if (attractG == 0.0) {
	  Fr = Fa; // if we have only repulsive forces, we can compute them directly into Fa
  }
  else if (repelG){
	  for(Fri = Fr, Fe = Fr + (ptse-pts); Fri != Fe; Fri++)
		  Fri->x = Fri->y = 0.0;
  }


  int count = 0;
  int degree = 0;
  int ind;
  int ind2;
  double dx, dy, dx2, dy2;
  double r2;

  if (repelG){
	  for(ptsi = pts, Fri = Fr, Fai = Fa, degreeVecti = degreeVect, i = 0; ptsi != ptse; ptsi++, Fri++, Fai++, degreeVecti++, i++) {

		  degree = *degreeVecti;

		  for(edgesIndsi = edgesInds+count; edgesIndsi < edgesInds+count+degree; edgesIndsi++){
			  ind = *edgesIndsi - 1;
			  (pts2 + ind)->x = ptsi->x;
			  (pts2 + ind)->y = ptsi->y;
			  //			  Rcout << "Attractive forces_x are: " <<  ind << std::endl;
		  }

		  for(ptsi2 = pts2+i, Fri2 = Fri; ptsi2 != pts2 + (ptse-pts); ptsi2++, Fri2++) {
			  dx = ptsi->x - ptsi2->x;
			  dy = ptsi->y - ptsi2->y;
			  r2 = sqr(dx) + sqr(dy);
			  if (r2 < 1e-6)
				  continue;

			  double fct = 0;
			  switch (law) {
			  case 0:
				  fct = 1 / r2;
				  break;
				  //            case TPNN::Linear:
				  //              fct = 1;
				  //              break;
			  case 1:
				  fct = 1 / (r2 * sqrt(r2));
				  //              break;
				  //            case TPNN::InverseExponential:
				  //              fct = 1 / (exp(r2/sigma2) - 1);
				  //              break;
				  //            case TPNN::KNN:
				  //              fct = sqrt(r2) * exp(-r2/sigma2);
				  //              break;
			  }

			  const double druvx = dx * fct;
			  Fri->x  += druvx;
			  Fri2->x -= druvx;

			  const double druvy = dy * fct;
			  Fri->y  += druvy;
			  Fri2->y -= druvy;
		  }

		  // Restore original values
		  if ((attractG == 0.0) ) {
			  for(edgesIndsi = edgesInds+count; edgesIndsi < edgesInds+count+degree; edgesIndsi++){
				  ind2 = *edgesIndsi - 1;
				  (pts2 + ind2)->x = (pts + ind2)->x;
				  (pts2 + ind2)->y = (pts + ind2)->y;
			  }
		  }
		  else {

			  for(edgesIndsi = edgesInds+count, edgeWeightsi = edgeWeights+count; edgesIndsi < edgesInds+count+degree; edgesIndsi++, edgeWeightsi++){
				  ind2 = *edgesIndsi - 1;
				  TPoint *pts2_temp = pts2 + ind2;
//				  TPoint *Fa_temp = Fa + ind2;

				  pts2_temp->x = (pts + ind2)->x;
				  pts2_temp->y = (pts + ind2)->y;
				  dx2 = ptsi->x - pts2_temp->x;
				  dy2 = ptsi->y - pts2_temp->y;
				  Fai->x = Fai->x + (dx2 * *edgeWeightsi);
				  Fai->y = Fai->y + (dy2 * *edgeWeightsi);
//				  Fa_temp->x = Fa_temp->x - dx2;
//				  Fa_temp->y = Fa_temp->y - dy2;
			  }
		  }

		  count = count + degree;

	  }
  }
  else{

	  for(ptsi = pts, Fai = Fa, degreeVecti = degreeVect, i = 0; ptsi != ptse; ptsi++, Fai++, degreeVecti++, i++) {

		  degree = *degreeVecti;

		  for(edgesIndsi = edgesInds+count, edgeWeightsi = edgeWeights+count; edgesIndsi < edgesInds+count+degree; edgesIndsi++, edgeWeightsi++){
			  ind2 = *edgesIndsi - 1;
			  TPoint *pts2_temp = pts + ind2;
//			  TPoint *Fa_temp = Fa + ind2;

			  dx2 = ptsi->x - pts2_temp->x;
			  dy2 = ptsi->y - pts2_temp->y;
			  Fai->x = Fai->x + (dx2 * *edgeWeightsi);
			  Fai->y = Fai->y + (dy2 * *edgeWeightsi);
//			  Fa_temp->x = Fa_temp->x - dx2;
//			  Fa_temp->y = Fa_temp->y - dy2;
		  }

		  count = count + degree;

	  }

  }

  // if both types of forces are used, balance them and mix them into Fa in the right proportions
  if ((repelG != 0.0) && (attractG != 0.0)) {
	  double repelGk;

	  if (dynamicBalancing) {
		  double FrTot = 0;
		  for(Fri = Fr, Fe = Fr + (ptse-pts); Fri != Fe; Fri++)
			  FrTot += sqr(Fri->x) + sqr(Fri->y);

		  double FaTot = 0;
		  for(Fai = Fa, Fe = Fa + (ptse-pts); Fai != Fe; Fai++)
			  FaTot += sqr(Fai->x) + sqr(Fai->y);

		  repelGk = FrTot > 0.001 ? repelG * fabs(FaTot / FrTot) : repelG;
    }
    else
      repelGk = repelG;

    for(Fai = Fa, Fri = Fr, Fe = Fr + (ptse-pts); Fri != Fe; Fai++, Fri++) {
      Fai->x = attractG * Fai->x  +  repelGk * Fri->x;
      Fai->y = attractG * Fai->y  +  repelGk * Fri->y;
    }
  }
}



/* Given coordinates of anchors and the symmetry type, it rotates the anchors
   so that the first lies at phi=0 and the second (if symmetry==2) is on the
   upper half-plane */

void symmetricTransformationGraph(TPoint *anc, TPoint *ance, bool mirrorSymmetry)
{
   const double phi = atan2(anc[0].y, anc[0].x);
   const double phi2 = atan2(anc[1].y, anc[1].x);

   const int sign = mirrorSymmetry && ((phi2<phi) || (phi2-phi > 3.1419265)) ? -1 : 1;
   const double dphi = /*3.1419265/2.0*/ - phi;
   const double cs = cos(dphi), sn = sin(dphi);

   for(TPoint *anci = anc; anci != ance; anci++) {
	 double anciyOld = anci->y;
     anci->y = sign * (anci->x * sn + anci->y * cs);
     anci->x =        (anci->x * cs - anciyOld * sn);
   }
}





// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::mat optimizeAnchorsGraph(arma::mat scaledData, NumericVector RedgesInds, NumericVector RedgeWeights, NumericVector RdegreeVect, arma::mat anchors, double attractG, double repelG, int law, int steps, bool normalizeExamples)
{

    double *Xi, *X;            // values of the chosen attributes
    double *edgeWeightsi, *edgeWeights;
    int nAttrs, nExamples, nEdges, i, dynamicBalancing, mirrorSymmetry;
    int *degreeVecti, *degreeVect, *edgesIndsi, *edgesInds;// number of (chosen) attributes and of examples
    TPoint *anci, *anc, *ance; // anchor coordinates
//    PyObject **lli, **ll;      // anchor labels
//    double minClass, maxClass; // minimal and maximal class values (for cont), #classes+1 (for disc)

//    // convert the examples, classes and anchors from Python lists
//    if (!loadRadvizData(scaledData, pyclasses, anchors, pyattrIndices, contClass, nAttrs, nExamples, X, classes, anc, ll, minClass, maxClass))
//      return PYNULL;
    // after loadRadVizData(), one will have: nExamples, nAttrs, X, classes, anc, ll, minClass, maxClass
//    OUTPUT:
//        nExamples      number of examples (length of 'scaledData'
//        nAttrs         number of attributes (length of 'anchors')
//        X              a list of attribute values (one-dimensional - flattened two-dim)
//                       - contains only the chosen attributes
//                       - for discrete classes, the list is sorted by class values
//        classes        for discrete classes, this is an (int *) with indices of class groups in X;
//                          therefore, the length of classes equals the number of classes+1
//                       for continuous, it contains the class values
//                       for MDS, it will return a pointer to symmatrix
//                       IMPORTANT: 'classes' should be freed by the caller for discrete and continuous,
//                                  but mustn't be freed for MDS
//        anc            anchor coordinates
//        ll             anchor labels (stored for when the anchor list needs to be reconstructed)
//        minClass       the minimal value encountered (for continuous only)
//        maxClass       for continuous classes it is the maximal value, for discrete it is the number of classes-1

    nAttrs = anchors.n_rows;
    nExamples = scaledData.n_cols;
    nEdges = RedgesInds.length();
    dynamicBalancing = 0;
    mirrorSymmetry = 1;
    arma::vec scaledDataVect = vectorise(scaledData);
//    arma::mat c(nExamples*nAttrs, 2);
//    arma::mat pts2;

    //Convert R-based data types to pointers (for now)
    anc = (TPoint *)malloc(nAttrs * sizeof(TPoint));
    X = (double *)malloc(nExamples * nAttrs * sizeof(double));
    edgeWeights = (double *)malloc(nEdges * sizeof(double));
    edgesInds = (int *)malloc(nEdges * sizeof(double));
    degreeVect = (int *)malloc(nExamples * sizeof(int));

    for(anci = anc, i = 0; i < nAttrs; i++, anci++) {
      anci->x = anchors(i,0);
	  anci->y = anchors(i,1);
    }

    for(edgesIndsi = edgesInds, edgeWeightsi = edgeWeights, i = 0; i < nEdges; i++, edgesIndsi++, edgeWeightsi++) {
      *edgesIndsi = RedgesInds[i];
      *edgeWeightsi = RedgeWeights[i];
    }

    for(Xi = X, i = 0; i < nExamples*nAttrs; i++, Xi++) {
      *Xi = scaledDataVect[i];
    }

    for(degreeVecti = degreeVect, i = 0; i < nExamples; degreeVecti++, i++) {
      *degreeVecti = RdegreeVect[i];
    }

    ance = anc + nAttrs;

    TPoint *danci, *danc = (TPoint *)malloc(nAttrs * sizeof(TPoint)), *dance = danc + nAttrs;   // anchors' moves
    TPoint *ptsi, *ptsi2, *pts = (TPoint *)malloc(nExamples * sizeof(TPoint)), *pts2 = (TPoint *)malloc(nExamples * sizeof(TPoint)), *ptse = pts + nExamples; // projections of examples
    TPoint *Fai, *Fa = (TPoint *)malloc(nExamples * sizeof(TPoint)), *Fae = Fa + nExamples;     // forces on examples
    TPoint *Fr = (attractG != 0.0) && (repelG != 0.0)
                     ? (TPoint *)malloc(nExamples * sizeof(TPoint)) : NULL;    // temporary array for computeForces
//
//    double *radi, *rad = NULL, *sumi, *sum = NULL, *sume;
//    if (normalizeExamples) {
//      rad = (double *)malloc(nAttrs * sizeof(double));                         // radii of anchors
//      sum = (double *)malloc(nExamples * sizeof(double));                      // sums of attr values for each example
//      sume = sum + nExamples;
//    }
//

    while (steps--) {
      // compute the projection
//      if (normalizeExamples) {
//        for(anci = anc, radi = rad; anci != ance; anci++, radi++)
//          *radi = sqrt(sqr(anci->x) + sqr(anci->y));
//
//        for(sumi = sum, Xi = X, ptsi = pts; ptsi != ptse; sumi++, ptsi++) {
//          ptsi->x = ptsi->y = *sumi = 0.0;
//          for(anci = anc, radi = rad; anci != ance; anci++, Xi++, radi++) {
//            ptsi->x += *Xi * anci->x;
//            ptsi->y += *Xi * anci->y;
//            *sumi += *Xi * *radi;
//          }
//          if (fabs(*sumi) > 1e-6) {
//            ptsi->x /= *sumi;
//            ptsi->y /= *sumi;
//          }
//          else
//            *sumi = 1.0; // we also use *sumi later
//        }
//      }
//        else {
          for(Xi = X, ptsi = pts, ptsi2 = pts2; ptsi != ptse; ptsi++, ptsi2++) {
            ptsi->x = ptsi->y = 0.0;
            for(anci = anc; anci != ance; anci++, Xi++) {
              ptsi->x += *Xi * anci->x;
              ptsi->y += *Xi * anci->y;
            }
            ptsi2->x = ptsi->x;
			ptsi2->y = ptsi->y;
          }
//        }
//    	Alternative code for just above (but see below for matrix-based alternative)
//      else {
//        for(i = 0, ptsi = pts; ptsi != ptse; ptsi++) {
//        	ptsi->x = ptsi->y = 0.0;
//          for(anci = anc; anci != ance; anci++, i++) {
//            ptsi->x += scaledDataVect[i] * anci->x;
//            ptsi->y += scaledDataVect[i] * anci->y;
//          }
//          c((i-1)/2,0) = ptsi->x;
//          c((i-1)/2,1) = ptsi->y;
//        }
//
//        pts2 = scaledData.t()*anchors; # More efficient computation

//
//
//      switch (contClass) {
//        case 0:
          int sigma2 = 1; //Currently not used, as we don't consider Gaussian law. Only added for compatibility
          computeForcesDiscreteGraph(pts, pts2, ptse, edgesInds, edgeWeights, degreeVect, law, sigma2, attractG, repelG, dynamicBalancing != 0, Fa, Fr, nEdges);
//          break;
//        case 1:
//          computeForcesContinuous(pts, ptse, (double *)classes, law, sigma2, Fa);
//          break;
//        case 2:
//          computeForcesMDS(pts, ptse, (TSymMatrix *)classes, law, sigma2, Fa);
//          break;
//      };
//
//      // Normalize forces if needed (why?! instead of dividing each *Xi?)
//      if (normalizeExamples)
//        for(Fai = Fa, sumi = sum; Fai != Fae; Fai++, sumi++) {
//          Fai->x /= *sumi;
//          Fai->y /= *sumi;
//        }
//

      // Transmit forces on particles to the anchors
      for(danci = danc; danci != dance; danci++)
        danci->x = danci->y = 0.0;

      for(Fai = Fa, Xi = X; Fai != Fae; Fai++) {            // loop over examples
        for(danci = danc; danci != dance; danci++, Xi++) {  // loop over anchors
          danci->x += Fai->x * *Xi;
          danci->y += Fai->y * *Xi;
        }
      }


      // Scale the changes (the largest is anchor move is 0.1*radius)
      double scaling = 1e10;
      for(anci = anc, danci = danc; danci != dance; anci++, danci++) {
        double maxdr = 0.01 * (sqr(anci->x) + sqr(anci->y));
        double dr = sqr(danci->x) + sqr(danci->y);
        if (scaling * dr > maxdr)
          scaling = maxdr / dr;
      }

      scaling = sqrt(scaling);
      for(danci = danc; danci != dance; danci++) {
        danci->x *= scaling;
        danci->y *= scaling;
      }


      // Move anchors
      for(anci = anc, danci = danc; danci != dance; danci++, anci++) {
        anci->x +=  danci->x;
        anci->y +=  danci->y;
      }


      // Center anchors (so that the average is in the middle)
      double aax = 0.0, aay = 0.0;
      for(anci = anc; anci != ance; anci++) {
        aax += anci->x;
        aay += anci->y;
      }
      aax /= nAttrs ? nAttrs : 1;
      aay /= nAttrs ? nAttrs : 1;

      for(anci = anc; anci != ance; anci++) {
        anci->x -= aax;
        anci->y -= aay;
      }


      // Scale (so that the largest radius is 1)
      double maxr = 0.0;
      for(anci = anc; anci != ance; anci++) {
        const double r = sqr(anci->x) + sqr(anci->y);
        if (r > maxr)
          maxr = r;
      }

      if (maxr > 0.001) {
        maxr = sqrt(maxr);
        for(anci = anc; anci != ance; anci++) {
          anci->x /= maxr;
          anci->y /= maxr;
        }
      }
    }
symmetricTransformationGraph(anc, ance, mirrorSymmetry != 0);
//
//   anchors = PyList_New(nAttrs);
//   for(i = 0, anci = anc, lli = ll;i < nAttrs; lli++, i++, anci++)
//     PyList_SetItem(anchors, i, *lli ? Py_BuildValue("ddO", anci->x, anci->y, *lli) : Py_BuildValue("dd", anci->x, anci->y));

//    //    Some output test code:
//    NumericVector pointsReal(2);
//    pointsReal[0] = pts->x;
//    pts++;
//    pointsReal[1] = pts->y;

//   return anchors;
//
//// PyCATCH;

//arma::mat points(nExamples,2);
//
//for(Fai = Fa, ptsi = pts, i = 0; i < nExamples; i++, ptsi++, Fai++) {
//	points(i,0) = ptsi->x;
//	points(i,1) = ptsi->y;
//}
//
//arma::mat forces(nExamples,2);
//
//for(Fai = Fa, i = 0; i < nExamples; i++, Fai++) {
//	forces(i,0) = Fai->x;
//	forces(i,1) = Fai->y;
//}


arma::mat newAnchors(nAttrs, 2);

for(anci = anc, i = 0; i < nAttrs; i++, anci++) {
	newAnchors(i,0) = anci->x;
	newAnchors(i,1) = anci->y;
}

//   // Free up memory
   free(X);
   free(anc);
////   free(ll);
////   if (contClass < 2)
//   free(classes);
//
   free(danc);
   free(Fa);
   free(Fr);
////   xfree(Fr);
////   xfree(sum);
////   xfree(rad);
   free(edgeWeights);
   free(edgesInds);
   free(pts);
   free(pts2);
   free(degreeVect);


	return(newAnchors);
}



