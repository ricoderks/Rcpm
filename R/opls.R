opls <- function(Xi, Yi, prep, A, ncox, ncoy, nrcv) {
###--- Written by E.Nevedomskaya 2007-2013---###
###--------------------------------------------------
# Based on malab script by O. Cloarec	Imperial College, London
###-------------------------------------------------
# Input:
# Xi : X matrix input
# Yi : Y column input
# prep : preprocessing available
#           - 'no' : no preprocessing
#           - 'mc' : meancentering
#           - 'uv' : Univariance scaling
#           
# A : number of correlated components, it can be determinated by PCA of Y'X
# ncox : number of orthogonal components in X
# ncoy : number of orthogonal component in Y
# number of fold in the cross-validation (full cross validation)
# Output:
# List with the following elements:
#	$MeanX   		=	variable means for the X table  
#	$MeanY  		=	variable means for the Y table      
#	$VarX  			=	variance of the variables in the X table    
#	$VarY   		=	variance of the variables in the Y table     
#	$svd       		=	results of the SVD transformation of the X-Y covariance table
#	$T         		=	T scores
#	$Ts        		=	X scores
#	$E         		= 	residuals
#	$wo 			=	Y-orthogonal loadings (non-normalized)       		
#	$TYosc			=	orthogonal scores     		
#	$PYosc     		=	Y-orthogonal loadings	
#	$Wosc   		=	orthogonal weughts   
#	$R2Xyo     		=	modelled percentage of X orthogonal to Y
#	$R2Xcorr   		=	modelled percentage of X correlated to Y
#	$R2XcorrVar		=	modelled percentage of X variance correlated to Y
#	$R2X			=	total percentage of X that had been modelled     
#	$U				=	U scores         
#	$F         		=	residuals
#	$R2Ycorr   		=	modelled percentage of Y correlated to X
#	$R2Y			=	total percentage of X that had been modelled        
#	$P         		=	loadings
#	$Bu 			=	regression coefficients       
#	$Bt        		=	regression coefficients
#	$Yhat 			=	Y predicted by the regression     
#	$R2Yhat    		=	predicted by the regression variance of Y
#	$R2Yhatcum 		=	cumulative predicted (by the regression) variance of Y
#	$R2Xhat    		=	predicted (by the regression) variance of X
#	$R2Xhatcum 		=	cumulative predicted variance of X
#	$Tcv   			=	cross-validated T-scores    
#	$TYosccv   		=	cross-validated Y-orthogonal scores
#	$Ucv       		=	cross-validated T-scores
#	$YPRESSf 		=	cumulative predicted residual sums of squares (PRESS) of Y
#	$YPRESSi   		=	PRESS for each column of Y
#	$XPRESSf		=	cumulative predicted residual sums of squares (PRESS) of X   
#	$XPRESSi  		=	PRESS for each column of X 
#	$Yhatcv    		=	Y predicted in cross-validation (CV)
#	$Xhatcv    		=	X predicted in cross-validation
#	$Q2Yhatcum 		=	cumulative predicted in CV variance of Y
#	$Q2Yhat    		= 	variance of Y predicted in CV
#	$Q2Xhatcum 		=	cumulative predicted in CV variance of X
#	$Q2Xhat    		= 	variance of Y predicted in CV

	model <- c();
	model$Wosc <- c();
	model$TYosc <- c();
	model$PYosc <- c();
	model$Cosc <- c();
	model$UXosc <- c();
	model$PXosc <- c();
	
	dimx <- dim(Xi);
	dimy <- dim(Yi);
	
#if (dimx[1]!=dimy[1]) stop("Number of samples are different in X and Y")
	
	nsx <- dimx[1];
	
	model$MeanX <- colMeans(Xi);
	model$MeanY <- colMeans(Yi);
	
	model$VarX <- apply(Xi,2,var);
	model$VarY <- apply(Yi,2,var);
	
# Preprocessing
	if (prep == "no") {
		X <- Xi;
		Y <- Yi;
	}
	
	if (prep == "mc") {
		X <- scale(Xi, scale=F);
		Y <- scale(Yi, scale=F);
	}	
	
	if (prep == "uv") {
		X <- scale(Xi);
		Y <- scale(Yi);
	}	
	
	#else stop("unknown preprocessing")
	
	SSX <- sum(X^2);
	SSY <- sum(Y^2);
	
	CSX <- colSums(X^2);
	CSY <- colSums(Y^2);
	
# Calculate the PCA components of Y'T
	
	CV <- t(Y) %*% X;
	
	model$svd <- svd(t(CV));
	model$svd$d <- diag(model$svd$d);
	
# Y predictive loading component
#model$svd$v <- t(model$svd$v[1:A,])
	model$svd$v <- t(t(model$svd$v[, 1:A]));
# X predictive loading component
	model$svd$u <- model$svd$u[, 1:A];
# Eigenvalue of CV
	model$svd$d <- model$svd$d[1:A, 1:A];
	model$wo <- c();
	
# Modelisation of X +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	
	Xr <- X;
	
#Projection of Xr on U in order to get the scores
	model$T <- Xr %*% model$svd$u;
	model$Ts <- model$T;
	
	model$E <- X-model$T %*% t(model$svd$u);
		
# Sequential removal ncox orthogonal components
	if (ncox != 0){
		for (i in 1:ncox) {
			# PCA of E'T for extraction of the first residual orthogonal component
			m <- pcaMethods::nipalsPca(t(model$E) %*% model$T, nPcs=1);
			
			# Extraction of Wosc from the PCA model m
			wosc <- m@scores;
			model$wo <- cbind(model$wo, wosc);
			# Normalisation wosc to unit length
			wosc <- wosc/as.numeric(sqrt(t(wosc) %*% wosc));
			
			# Projection of Xr on the wosc axe in order to get the orthogonal scores     
			tosc <- Xr %*% wosc / as.numeric(t(wosc) %*% wosc);
			
			# Projection of tosc in Xr in order to get the Y-orthogonal loading
			pyosc <- t(Xr) %*% tosc / as.numeric(t(tosc) %*% tosc);
			
			# Removal of the orthogonal component from Xr
			Xr <- Xr - tosc %*% t(pyosc);
			
			# Update of T from the residual Xr
			model$T <- Xr %*% model$svd$u;
			model$Ts <- cbind(model$Ts, model$T);
			# Storage of the orthogonal components
			model$TYosc <- cbind(model$TYosc, tosc);
			model$PYosc <- cbind(model$PYosc, pyosc);
			model$Wosc <- cbind(model$Wosc, wosc);
			
			# Update of the residual E
			model$E <- X-model$T %*% t(model$svd$u) - model$TYosc %*% t(model$PYosc);
			
		}
	}
	
# Model statistics corresponding to the X modelling
	
# Percentage of X orthogonal to Y
	if (ncox != 0) {
		model$R2Xyo <- sum((model$TYosc %*% t(model$PYosc)) * (model$TYosc %*% t(model$PYosc))) / SSX;
	}
	
# Percentage of X correlated to Y
	model$R2Xcorr <- sum((model$T %*% t(model$svd$u))*(model$T %*% t(model$svd$u))) / SSX;
	model$R2XcorrVar <- colSums((model$T %*% t(model$svd$u)) * (model$T %*% t(model$svd$u))) / CSX;
# Total percentage of X that had been modelled
	model$R2X <- 1 - sum(colSums((model$E)^2) / SSX);
	
#End of X modelisation+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	
# Modelisation of Y +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   
	
	Yr <- Y;
	
# Projection of Yr on C to get the scores
	model$U <- Yr %*% model$svd$v;
# Removal of this part of Y correlated to X
	model$F <- Y-model$U %*% t(model$svd$v);
	
# Sequential removal of the structured noise of Y
	
	if (ncoy != 0){
		for (i in 1:ncoy) {
			# PCA of F'U for extraction of the first residual orthogonal component
			m <- pcaMethods::nipalsPca(t(t(model$F) %*% model$U), nPcs=1);
			
			# Extraction of Cosc from the PCA model m
			cosc <- m@loadings;
			
			# Normalisation cosc to unit length    
			cosc <- cosc / as.numeric(sqrt(t(cosc) %*% cosc));
			
			# Projection of Yr on the wosc axe in order to get the orthogonal scores     
			uosc <- Yr %*% cosc / as.numeric(t(cosc) %*% cosc);
			
			#Projection of uosc in Yr in order to get the X-orthogonal loading
			pxosc <- t(Yr) %*% uosc / as.numeric(t(uosc) %*% uosc);
			
			# Removal of the orthogonal component from Xr
			Yr <- Yr - uosc %*% t(pxosc);
			
			# Update of U from the residual Yr
			model$U <- Yr %*% model$svd$v;
			
			# Storage of the orthogonal components    
			model$UXosc <- cbind(model$UXosc, uosc);
			model$Cosc <- cbind(model$Cosc, cosc);
			model$PXosc <- cbind(model$PXosc, pxosc);
			
			#Update of the residual E
			model$F <- Y - model$U %*% t(model$svd$v) - model$UXosc %*% t(model$PXosc);
		}
	}
# Model statistics corresponding to the X modelling
	
# Percentage of Y orthogonal to X
	if (ncoy != 0) {
		model$R2Yxo <- sum(colSums((model$UXosc %*% t(model$PXosc)) * (model$UXosc %*% t(model$PXosc)))) / SSY;
	}
	
# Percentage of Y correlated to X
	model$R2Ycorr <- sum(colSums((model$U %*% t(model$svd$v)) * (model$U %*% t(model$svd$v)))) / SSY;
	
# Total percentage of Y that had been modelled
	model$R2Y <- 1 - sum(colSums((model$F) * (model$F))) / SSY;
	model$P <- t(model$T) %*% Xr / c(t(model$T) %*% model$T);
	model$P <- t(model$P);
	
#End of Y modelisation+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	
# O2PLS Regression Coefficients
	
	model$Bu <- solve((t(model$U) %*% model$U)) %*% t(model$U) %*% model$T;
	model$Bt <- solve(t(model$T) %*% model$T) %*% t(model$T) %*% model$U;
	
# Fitting characteristics for Y
	model$Yhat  <- model$T %*% model$Bt %*% t(model$svd$v);
	
	model$R2Yhat <- 1 -(colSums((model$Yhat - Y) * (model$Yhat - Y))) / CSY;
	model$R2Yhatcum <- 1 - sum(colSums((model$Yhat - Y) * (model$Yhat - Y))) / SSY;
	
# Fitting characteristics for X
	Xhat <- model$U %*% model$Bu %*% t(model$svd$u);
	
	model$R2Xhat <- 1 - colSums((Xhat - X) * (Xhat - X)) / CSX;
	model$R2Xhatcum <- 1 - sum(colSums((Xhat - X) * (Xhat - X))) / SSX;
	
	Yhat_cross <- matrix(nrow=nrow(Y), ncol=ncol(Y));
	model$Tcv <- matrix(nrow=nrow(model$T), ncol=ncol(model$T));
	if (ncox != 0) {
		model$TYosccv <- matrix(nrow=nrow(model$TYosc), ncol=ncol(model$TYosc));
	}
	
	Xhat_cross <- matrix(nrow=nrow(X), ncol=ncol(X));
	model$Ucv <- matrix(nrow=nrow(model$U), ncol=ncol(model$U));
	if (ncoy != 0) {
		model$UXosccv <- matrix(nrow=nrow(model$UXosc), ncol=ncol(model$UXosc));
	}
	
	if (nrcv > 0) {
		for (k in 1:nrcv) {
			# Definition of the training and test sets
			
			# For X:
			Xtest <- X[seq(k, nsx, nrcv), ];        
			Xtrain <- X[setdiff(1:nsx, seq(k, nsx, nrcv)), ];
			
			# For Y:
			Ytest <- cbind(Y[seq(k, nsx, nrcv), ]);
			Ytrain <- cbind(Y[setdiff(1:nsx, seq(k, nsx, nrcv)), ]);
			
			# Predictive model from training set
			mcv <- opls(Xtrain, Ytrain, 'no', A, ncox, ncoy, 0);
			
			# Prediction of the test set X t Y
			modelPredY <- opls_pred(Xtest, mcv, "xy");
			Yhat_cross[seq(k, nsx, nrcv), ] <- modelPredY$Yhat;
			
			# Extraction of the cross-validated scores and loadings
			model$Tcv[seq(k, nsx, nrcv), ] <- modelPredY$T;
			model$TYosccv[seq(k, nsx, nrcv), ] <- modelPredY$To;
			
			# Prediction of the test set Y to X
			modelPredX <- opls_pred(Ytest, mcv, "yx");
			Xhat_cross[seq(k, nsx, nrcv), ] <- modelPredX$Xhat;
			
			
			# Extraction of the cross-validated scores and loadings
			model$Ucv[seq(k, nsx, nrcv), ] <- modelPredX$U;
			model$UXosccv[seq(k, nsx, nrcv), ] <- modelPredX$Uo;
		}
		
		
		
		# Prediction Residual Sum of Squares (PRESS)
		# for Y
		# Cumulative
		model$YPRESSf <- sum(colSums((Y - Yhat_cross)^2));
		# For each column of Y
		model$YPRESSi <- colSums((Y - Yhat_cross)^2);
		
		# for X
		# Cumulative
		model$XPRESSf <- sum(colSums((X - Xhat_cross)^2));
		# For each column of X
		model$XPRESSi <- colSums((X - Xhat_cross)^2);
		
		# Predicted X and Y from CV
		model$Yhatcv <- Yhat_cross;
		model$Xhatcv <- Xhat_cross;
		
		model$Q2Yhatcum <- 1 - model$YPRESSf / SSY;
		model$Q2Yhat <- 1 - model$YPRESSi / CSY;
		
		model$Q2Xhatcum <- 1 - model$XPRESSf/SSX;
		model$Q2Xhat <- 1 - model$XPRESSi / CSX;
		
		model$Ucv <- Xhat_cross %*%t (model$Bu %*% t(model$svd$u)) %*% solve((model$Bu %*% t(model$svd$u)) %*% t(model$Bu %*% t(model$svd$u)));
	}
	
	return(model)
}