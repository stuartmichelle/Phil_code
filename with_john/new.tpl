
DATA_SECTION
 // READ DATA
 !!ad_comm::change_datafile_name("new.dat");
 // Growth increment data
 init_int numGrowIncObs; // Number of growth increment observations
 init_matrix GrowIncDat(1,numGrowIncObs,1,4);	// Growth increment data
 !!cout<<"Read in the data"<<endl;
  
 // READ PARAMETER CONTROLS 
 !!ad_comm::change_datafile_name("new-switches.dat");
 // MODEL TYPE
 init_int modelType;
 // REFERENCE AGES
 init_number A_a;
 init_number A_b;

 // GROWTH PHASE CTRLS
 init_int La_PH;
 init_int Lb_PH;
 init_int kpar_PH;
 init_int bpar_PH;

 // Lsd PHASE CTRLS
 init_int Lsd_a_PH;
 init_int Lsd_b_PH;

 // TAGGING CORRELATION PHASE CTRLS
 init_int rho_k_PH;
 init_int rho_r0_PH;

 // RE PHASE CTRLS
 init_int remean_PH;
 init_int resd_PH;
 init_int re_PH;
  
 // LAMBDAS
 init_number tag_lambda;
 init_number re_lambda;
 
 !!cout<< "re_lambda " << re_lambda <<endl;  
 
PARAMETER_SECTION

 //GROWTH CURVE PARAMETERS
 // VBSchnute or Richardsab
 init_number La(La_PH);
 init_number Lb(Lb_PH);
 init_number kpar(kpar_PH);
 init_number bpar(bpar_PH);

 // TAGGING CORRELATION PARAMETERS
 init_bounded_number rho_k(0.0,5.0,rho_k_PH);
 init_bounded_number rho_r0(0.01,0.99,rho_r0_PH);

 // Lsd PARAMETERS
 init_bounded_number Lsd_a(0.1,35.0,Lsd_a_PH);		
 init_bounded_number Lsd_b(0.1,45.0,Lsd_b_PH);		

  // RE PARAMETERS
 init_number ln_rand_mean(remean_PH);	
 init_bounded_number ln_rand_sd(-15,15,resd_PH);
 random_effects_vector ln_Arand(1,numGrowIncObs,re_PH);

 objective_function_value objn;

PROCEDURE_SECTION

 int i;
 objn=0.;
 for(i=1;i<=numGrowIncObs;i++)
 {
     //cout << "i= " << i << endl;
     objn_cluster(i,ln_Arand(i),La,Lb,kpar,bpar,
		Lsd_a,Lsd_b,rho_r0,rho_k,
		ln_rand_mean,ln_rand_sd);
 }

SEPARABLE_FUNCTION void objn_cluster(int i, const dvariable& ln_Arand,  const dvariable& La, const dvariable& Lb, const dvariable& kpar, const dvariable& bpar, const dvariable& Lsd_a, const dvariable& Lsd_b, const dvariable& rho_r0, const dvariable& rho_k, const dvariable& ln_rand_mean, const dvariable& ln_rand_sd)
 int j;
 dvariable rand_sd;
 dvariable L1tag_pred;
 dvariable L2rec_pred;
 dvariable L1tag_sd;
 dvariable L2rec_sd;
 dvariable rho_t50;
 dvariable tagrho;
 dvariable tag_res1;
 dvariable tag_res2;
 dvariable tag_like;
 dvariable Arand;
 dvariable penRand;
 dvariable Linf;
 dvariable tzero;
 dvariable L2rec_bias;


 if(modelType==6)  // Richardsab
   {
	tzero=(1/kpar)*log(bpar*(pow(Lb,1.0/bpar) - pow(La,1.0/bpar))/
               (pow(La,1.0/bpar)*exp(-kpar*A_a)-pow(Lb,1.0/bpar)*exp(-kpar*A_b)));
 	Linf=La*pow(1.0 + (1.0/bpar)*exp(-kpar*(A_a-tzero)),bpar);
   }
 rho_t50=log(rho_r0/(1.-rho_r0))/rho_k;
 Arand=exp(ln_Arand);
 rand_sd=exp(ln_rand_sd);

	// Lpred
 if(modelType==5)   // VBSchnute
 {
   L1tag_pred=La + (Lb-La)*(1-exp(-kpar*(Arand-A_a)))/(1-exp(-kpar*(A_b-A_a)));
   L2rec_pred=La + (Lb-La)*(1-exp(-kpar*(Arand+GrowIncDat(i,4)-A_a)))/
 	       (1-exp(-kpar*(A_b-A_a)));
 }
 if(modelType==6)   // Richardsab
 {
   L1tag_pred=Linf*pow(1.0+(1.0/bpar)*exp(-kpar*(Arand-tzero)),-bpar);
   L2rec_pred=Linf*pow(1.0+(1.0/bpar)*
         exp(-kpar*(Arand+GrowIncDat(i,4)-tzero)),-bpar);
 }
 	 // Lsd
 L1tag_sd=Lsd_a + (Lsd_b - Lsd_a)*(L1tag_pred - La)/(Lb - La);
 L2rec_sd=Lsd_a + (Lsd_b - Lsd_a)*(L2rec_pred - La)/(Lb - La);
	 
 	 // Likelihoods for L1 and L2
 tagrho=1-1/(1+exp(-rho_k*(GrowIncDat(i,4)-rho_t50)));
 tag_res1=(GrowIncDat(i,1)-L1tag_pred)/L1tag_sd;
 tag_res2=(GrowIncDat(i,2)-L2rec_pred)/L2rec_sd;
 tag_like = log(L1tag_sd)+log(L2rec_sd)+ 0.5*log(1-square(tagrho))+
 	 0.5*(square(tag_res1)+square(tag_res2)-
 	 2*tagrho*tag_res1*tag_res2)/(1-square(tagrho));
 //tag_like = log(L1tag_sd)+0.5*square(tag_res1);
 //tag_like += log(L2rec_sd)+0.5*square(tag_res2); 

	// penalty for Arand
 penRand = 0.5*square((ln_Arand-ln_rand_mean)/rand_sd)+ln_rand_sd;
 objn += tag_like*tag_lambda + penRand*re_lambda;

REPORT_SECTION

 report << "#==================== DATA  ====================" << endl;
 report << " " << endl;

  report << "# numGrowIncObs" << endl;
 report << numGrowIncObs << endl;
 report << " " << endl;

 report << "# GrowIncDat" << endl;
 report << GrowIncDat << endl;
 report << " " << endl;

 report << " " << endl;
 report << "#==================== CONTROLS AND SWITCHES  ====================" << endl;
 report << " " << endl;

 report << "# MODEL TYPE" << endl;
 report << modelType << endl;
 report << " " << endl;
 
 report << "# REFERENCE AGES" << endl;
 report << "# A_a" << endl;
 report <<  A_a << endl;
 report << "# A_b" << endl;
 report <<  A_b << endl;
 report << " " << endl;

 report << "# ESTIMATION PHASES - growth parameters" << endl;
 report << "# La_PH = " << La_PH << endl;
 report << "# Lb_PH = " << Lb_PH << endl;
 report << "# Kpar_PH = " << kpar_PH << endl;
 report << "# bpar_PH = " << bpar_PH << endl;
 report << " " << endl;

 report << "# ESTIMATION PHASES - tagging correlation parameters" << endl;
 report << "# rho_k_PH = " << rho_k_PH << endl;
 report << "# rho_r0_PH = " << rho_r0_PH << endl;
 report << " " << endl;
 
 report << "# ESTIMATION PHASES - Lsd parameters" << endl;
 report << "# Lsd_a_PH = " << Lsd_a_PH << endl;
 report << "# Lsd_b_PH = " << Lsd_b_PH << endl;
 report << " " << endl;
 
 report << "# LAMBDAS" << endl;
 report << "# tag_lambda = " << tag_lambda << endl;
 report << "# re_lambda = " << re_lambda << endl;
 report << " " << endl;

 report << " " << endl;
 report << "#==================== ESTIMATED PARAMETERS  ====================" << endl;
 report << " " << endl;

 report << "# La" << endl;
 report << La << endl;
 report << " " << endl;

 report << "# Lb" << endl;
 report << Lb << endl;
 report << " " << endl;

 report << "# kpar" << endl;
 report << kpar << endl;
 report << " " << endl;

 report << "# bpar" << endl;
 report << bpar << endl;
 report << " " << endl;
 
 report << "# rho_k" << endl;
 report << rho_k << endl;
 report << "# rho_r0" << endl;
 report << rho_r0 << endl;
 report << " " << endl;

 report << "# Lsd_a" << endl;
 report << Lsd_a << endl;
 report << " " << endl;
 
 report << "# Lsd_b" << endl;
 report << Lsd_b << endl;
 report << " " << endl;

 report << "# Tagging parameters" << endl;
 report << "# ln_Arand" << endl;
 report << ln_Arand << endl;
 report << " " << endl;
 
 report << "# ln_rand_mean" << endl;
 report << ln_rand_mean << endl;
 report << " " << endl;

 report << "# ln_rand_sd" << endl;
 report << ln_rand_sd << endl;
 report << " " << endl;

 report << " " << endl;
 report << "#================== DERIVED QUANTITIES  ====================" << endl;
 report << " " << endl;

 report << "#==================== LIKELIHOODS ====================" << endl;
 report << " " << endl;

 report << "# objn" << endl;
 report << objn << endl;
 report << " " << endl;
