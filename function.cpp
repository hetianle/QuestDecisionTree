//
// Created by tianle on 12/11/17.
//

#include "function.h"


Node::Node() {
    splitPoint=-1;
    isLeaf= false;
    leftChild=NULL;
    rightChild=NULL;
    classified=-1;
}

Node::Node(Table data_sta,VarVector var_sta) {
    splitPoint=-1;
    isLeaf= false;
    leftChild=NULL;
    rightChild=NULL;
    classified=-1;
    this->data_sta=data_sta;
    this->var_sel_sta=var_sta;
}

QuestDecisionTree::QuestDecisionTree() {
    // cout<<"Empty data!";
}

QuestDecisionTree::QuestDecisionTree(Table data,VarVector vars) {
    /*
     * Construction method of class QuestDecisionTree
     * */

    TrainingData=data;
    variables=vars;
    VariableNum=variables.size();


    Tree=trainingTree(TrainingData,this->variables); //call training method to construct tree.
//    for(int i=0;i<VariableNum;i++){
}

Variable QuestDecisionTree::variableSelection(Table data,VarVector variables) {
    /*
     * variable selection method
     * data         --Training data in this node.
     * variables    --Containing all th variables, but with different status, used or not.
     * return       --Selected variable of this node.
     * */

    VarVector unused;
    // int binVarNum = 0;
    int orderedVarNum = 0;
    int categVarNum = 0;
    double alpha1=-1;
    double alpha2=-1;

    double k1=-1;// initialize k1 with -1
    double k2=-1;// initialize k2 with -1
    double kpi=-1;
    double alpha=0.5; //Let α ∈ (0, 1) be a pre-specified level of significance.

    int K=-1;
    int K1=-1;

    int classNum=0;
    RecordDouble remainclasstype;
    for(int f=0;f<data.size();f++) {
        if (find(remainclasstype.begin(), remainclasstype.end(), data[f][data[f].size() - 1]) ==
            remainclasstype.end()) {
            remainclasstype.push_back(data[f][data[f].size() - 1]);
            classNum++;
        }
    }


    double mu1 = classNum - 1; //freedom mu1: classnumber - 1
    double mu2 = data.size() - mu1; //free dom mu2: casenumber- classnumber

    for (int i = 0; i < variables.size(); i++) {
        if (!variables[i].isUsed) {
            unused.push_back(variables[i]);

            switch (variables[i].type) {
                case 0 :
                    categVarNum++;
                    break;
                case 1:
                    orderedVarNum++;
                    K1++;
                    break;
                case 2:
                    categVarNum++;
                default:
                    break;
            }
        }
    }
    K=unused.size();
    if(K==1){
        return unused[0];
    }

    if (orderedVarNum >= 1) {
        //Statistic stas;
        RecordDouble Fvalues = Statistic::FvalueWithAnovaTest(data, variables);
        RecordDouble::iterator maxFvalueIndex = max_element(begin(Fvalues), end(Fvalues));
        k1=distance(begin(Fvalues),maxFvalueIndex); // set the index of the variable with max F value
        double Fk1 = *maxFvalueIndex; //the Fvalue of the variable
        if(Fk1>0) {
            alpha1 = gsl_cdf_fdist_P(mu1, mu2,
                                     Fk1); // the P value of the k1 th variable. this variable must be ordinal variable and of biggest F value.
        }else{
            alpha1=0;
        }
    }

    if(variables.size()>orderedVarNum){
        RecordDouble Pvalues;
        double min_p=1000000;
        //   int k2=-1;
        for(int j=0;j<variables.size();j++){

            if (!variables[j].isUsed && variables[j].type==0) {
                //double chisquare;
                double p=Statistic::ChiSquareTest (data,variables[j].id,data[0].size()-1);

                Pvalues.push_back(p);
                if(p<min_p){
                    min_p=p;
                    k2=j;
                }
            }
        }
        double Betak2=0;
        if(Pvalues.size()>0){
            Betak2=*min_element(Pvalues.begin(),Pvalues.end());
        } else{
            Betak2=0;
        }
        alpha2=Betak2;

    }

    double minalpha=-1;
    // if(alpha1>0 && alpha2>0){
    if(alpha1<=alpha2){
        minalpha=alpha1;
        kpi=k1;
        if(kpi<0){
            kpi=k2;
        }
    }else{
        minalpha=alpha2;
        kpi=k2;
        if(kpi<0){
            kpi=k1;
        }
    }
    //}

    if(minalpha>0 && minalpha < (alpha/(double)K)){
        return variables[kpi];
    }else{

        RecordDouble Fvalues2 = Statistic::FvalueAnovaTestWithAbs(data, variables);
        RecordDouble::iterator maxFvalueIndex2 = max_element(begin(Fvalues2), end(Fvalues2));
        double kpipi=distance(begin(Fvalues2),maxFvalueIndex2); // set the index of the variable with max F value
        double Fkpipi = *maxFvalueIndex2; //the Fvalue of the variable
//        double mu1 = 3 - 1; //freedom mu1: classnumber - 1
//        double mu2 = 151 - 3; //free dom mu2: casenumber- classnumber
        if (Fkpipi>0) {
            double alphazero = gsl_cdf_fdist_P(mu1, mu2, Fkpipi); // the


            if (alphazero < (alpha / (double) (K + K1))) {
                return variables[kpipi];
            } else {
                return variables[kpi];
            }
        } else{
            return variables[kpi];
        }
    }

}

double QuestDecisionTree::splitSelection(Table& Pdata,Variable splitVar) {

    /*
     * split point selection in the node.
     * Pdata        --the training data pointer of the node
     * splitVar     --the selected variables if the node.
     *
     * return       --split point.
     * */
    Table data=Pdata;
    if(splitVar.type==0){ //categorical
        int id=splitVar.id;


        // int n=data.size();
        RecordDouble column;
        for(int k=0;k<data.size();k++){
            column.push_back(data[k][id]);
            // data[k][id];
        }

        sort(column.begin(),column.end());

        Record column_int;
        for(int i=0;i<column.size();i++){
            column_int.push_back((int)column[i]);
        }
        Record::iterator end=unique(column_int.begin(),column_int.end());
        column_int.erase(end,column_int.end());

        int m=column_int.size();
        int n=data.size();

        gsl_matrix *Matrix_V=gsl_matrix_alloc(n,m);
        for(int i=0;i<n;i++){
            int tempvalue=data[i][id];
            // int location=*find(column.begin(),column.end(),tempvalue);
            for(int j=0;j<m;j++){
                if(j==(tempvalue-1)){
                    gsl_matrix_set (Matrix_V, i, j, 1.0);
                }else{
                    gsl_matrix_set (Matrix_V, i, j, 0.0);
                }
            }
        }


        gsl_matrix *Identity_I=gsl_matrix_alloc(n,n);
        for(int i=0;i<n;i++){
            for(int j=0;j<n;j++){
                if(i==j){
                    gsl_matrix_set (Identity_I, i, j, 1.0);
                } else {
                    gsl_matrix_set (Identity_I, i, j, 0.0);
                }
            }
        }
        gsl_matrix *Ncolumn_1=gsl_matrix_alloc(n,1);
        for(int i=0;i<n;i++){
            gsl_matrix_set(Ncolumn_1,i,0,1.0);
        }

        // H = I − N−1  11′
        gsl_matrix *Ncolumn_1_T=gsl_matrix_alloc(1,n);//
        for(int i=0;i<n;i++){
            gsl_matrix_set(Ncolumn_1_T,0,i,1.0);
        }

        //    MatrixOp::gsl_matrix_inverse(Ncolumn_1,Ncolumn_1_inv);
        gsl_matrix *Mul_N1_N1T=gsl_matrix_alloc(n,n);
        MatrixOp::gsl_matrix_mul(Ncolumn_1,Ncolumn_1_T,Mul_N1_N1T);

        gsl_matrix_scale(Mul_N1_N1T, -1/((double)n));

        gsl_matrix *Matrix_H=gsl_matrix_alloc(n,n);
        gsl_matrix_add(Matrix_H,Identity_I);
        gsl_matrix_add(Matrix_H, Mul_N1_N1T);

        gsl_matrix* Matrix_H_V=gsl_matrix_alloc(n,m);
        MatrixOp::gsl_matrix_mul(Matrix_H,Matrix_V,Matrix_H_V);

        gsl_vector * Vector_S=gsl_vector_alloc(m);

        gsl_vector * work;
        gsl_matrix *Matrix_P=Matrix_H_V;

        gsl_matrix *Matrix_Q=gsl_matrix_alloc(m,m);

        gsl_vector *workspace=gsl_vector_alloc(m);
        gsl_linalg_SV_decomp(Matrix_P, Matrix_Q,  Vector_S,  workspace);

        //singular value decomposition
        RecordDouble eigenvalues;
        for(int i=0;i<Vector_S->size;i++){
            double t=gsl_vector_get(Vector_S,i);
            eigenvalues.push_back(t);
        }

        int Max_M_N;
        if(m>n){
            Max_M_N=m;
        } else{
            Max_M_N=n;
        }

        // Define an eigenvalue dm as ‘positive’ if it satisfies dm > max(M,N)*d1*ε, and as ‘zero’ otherwise
        double d1=eigenvalues[0];
        double ε=1/pow(2,52);
        double max_M_N_d1_ε=Max_M_N*d1*ε;

        int Count_Positive=0,Count_Zero=0;

        for(int i=0;i<eigenvalues.size();i++){
            if(eigenvalues[i]>max_M_N_d1_ε){
                Count_Positive++;
            }else{
                Count_Zero++;
            }
        }
        double Rank=Count_Positive;
        if(Rank==0){
            Rank=eigenvalues.size();
        }
        gsl_matrix* Matrix_F=gsl_matrix_alloc(m,Rank);
        gsl_matrix* Matrix_U=gsl_matrix_alloc(Rank,Rank);
        for(int i=0;i<m;i++){
            for(int j=0;j<Rank;j++){
                //   gsl_matrix_get
                gsl_matrix_set(Matrix_F,i,j,gsl_matrix_get(Matrix_Q,i,j));

                if(i<Rank && j<Rank){
                    if(i==j){
                        gsl_matrix_set(Matrix_U,i,j,1/eigenvalues[i]);
                    }else{
                        gsl_matrix_set(Matrix_U,i,j,0.0);
                    }
                }
            }
        }

        gsl_matrix* Matrix_F_T;
        Matrix_F_T=MatrixOp::gsl_matrix_transpose(Matrix_F);

        gsl_matrix* Matrix_V_REDUCTION=gsl_matrix_alloc(24,151);
        MatrixOp::gsl_matrix_mul(Matrix_F_T,MatrixOp::gsl_matrix_transpose(Matrix_V),Matrix_V_REDUCTION);

        vector<RecordDouble> _means;
        for(int i=0;i<m;i++){
            _means.push_back(*(new RecordDouble));
        }

        for(int i=0;i< (int)(Matrix_V->size1);i++){

            for(int j=0;j<(int)Matrix_V->size2;j++){
                _means[j].push_back(gsl_matrix_get(Matrix_V,i,j));
            }
        }

        for (int i=0;i<_means.size(); i++){
            double sum=0;
            for(int j=0;j<_means[i].size();j++)
                sum+=_means[i][j];
            double fenmu=_means[i].size();
            double me=sum/fenmu;
            _means[i].push_back(me);

        }

        for(int i=0;i<_means.size();i++){
            for(int j=0;j<_means[i].size()-1;j++){
                _means[i][j]-=_means[i][_means[i].size()-1];

            }
        }

        gsl_matrix* Matrix_G;
        gsl_matrix* Matrix_G_T=gsl_matrix_alloc(m,n);

        for(int i=0;i<_means.size();i++){
            for(int j=0;j<_means[i].size()-1;j++){
                _means[i][j]-=_means[i][_means[i].size()-1];
                gsl_matrix_set(Matrix_G_T,i,j,_means[i][j]);

            }
        }
        Matrix_G=MatrixOp::gsl_matrix_transpose(Matrix_G_T);
        gsl_matrix* Matrix_B=gsl_matrix_alloc(m,m);
        MatrixOp::gsl_matrix_mul(Matrix_G_T,Matrix_G,Matrix_B);

        gsl_matrix* Matrix_G_cheng_F=gsl_matrix_alloc(Matrix_G->size1,Matrix_F->size2);
        MatrixOp::gsl_matrix_mul(Matrix_G,Matrix_F,Matrix_G_cheng_F);

        gsl_matrix* Matrix_GFU=gsl_matrix_alloc(Matrix_G_cheng_F->size1,Matrix_U->size2);
        MatrixOp::gsl_matrix_mul(Matrix_G_cheng_F,Matrix_U,Matrix_GFU);

        if(splitVar.id==3){
            //   printf("breakkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk");
        }

        gsl_matrix* Matrix_A2=Matrix_GFU;

        // printf("break");

        gsl_matrix* Matrix_vvvvv=gsl_matrix_alloc(Matrix_A2->size2,Matrix_A2->size2);
        gsl_vector* eligencessss=gsl_vector_alloc(Matrix_A2->size2);
        gsl_vector* workkkkkkkkk=gsl_vector_alloc(Matrix_A2->size2);


        gsl_linalg_SV_decomp(Matrix_A2,Matrix_vvvvv,eligencessss,workkkkkkkkk);

        gsl_matrix* eligenvector_with_max_value=gsl_matrix_alloc(Matrix_vvvvv->size1,1);
        for(int i=0;i<eligenvector_with_max_value->size1;i++){
            gsl_matrix_set(eligenvector_with_max_value,i,0,gsl_matrix_get(Matrix_vvvvv,i,0));
        }

        // Transform each v to ξ = a′Uy = a′UF′v
        gsl_matrix* eligenvector_T=MatrixOp::gsl_matrix_transpose(eligenvector_with_max_value);

        gsl_matrix* Matrix_A_T_cheng_U=gsl_matrix_alloc(eligenvector_T->size1,Matrix_U->size2);
        MatrixOp::gsl_matrix_mul(eligenvector_T,Matrix_U,Matrix_A_T_cheng_U);

        gsl_matrix* Mat_AT_U_FT=gsl_matrix_alloc(Matrix_A_T_cheng_U->size1,Matrix_F_T->size2);
        MatrixOp::gsl_matrix_mul(Matrix_A_T_cheng_U,Matrix_F_T,Mat_AT_U_FT);

        gsl_matrix* Mat_AT_U_FT_VT=gsl_matrix_alloc(Mat_AT_U_FT->size1,MatrixOp::gsl_matrix_transpose(Matrix_V)->size2);
        MatrixOp::gsl_matrix_mul(Mat_AT_U_FT,MatrixOp::gsl_matrix_transpose(Matrix_V),Mat_AT_U_FT_VT);

        Table Transformed_data=data;
        for(int i=0;i<(Transformed_data).size();i++){

            (Transformed_data)[i][splitVar.id]=gsl_matrix_get(Mat_AT_U_FT_VT,0,i);

        }
        //   TrainingData =Transformed_data;
        Pdata=Transformed_data;
        double d=SplitSelection_Ordered(Transformed_data,splitVar);

        return d;



    } else { //ordernal variable
        double d=SplitSelection_Ordered(data,splitVar);
        return d;
    }

}

Node* QuestDecisionTree::get_tree(){
    return this->Tree;
}

double QuestDecisionTree::SplitSelection_Ordered(Table data,Variable splitVar) {
    /*
     * split point selection for ordered variables
     * data     --Trainging data of the node.
     * splitVar --Split variable of the node.
     *
     * */
    Kmeans *pKmeans = new Kmeans(2, data);
    Table clustereddata = pKmeans->clustereddata;
    int CountClass0 = 0;
    double sum_class0=0;
    double sum_class1=0;
    int CountClass1 = 0;

    double P_A_Givent=0;
    double P_B_Givent=0;
    double mean_classA=0;
    double mean_classB=0;
    double variance_A=0;
    double variance_B=0;

    RecordDouble class_0;
    RecordDouble class_1;

    for (int i = 0; i < clustereddata.size(); i++) {
        if (clustereddata[i][6] == 0) {
            CountClass0++;
            sum_class0+=clustereddata[i][splitVar.id];
            class_0.push_back(clustereddata[i][splitVar.id]);
        } else {
            CountClass1++;
            sum_class1+=clustereddata[i][splitVar.id];
            class_1.push_back(clustereddata[i][splitVar.id]);
        }
    }




    if(CountClass0>CountClass1){
        P_A_Givent=CountClass0/(double)(CountClass0+CountClass1);
        P_B_Givent=1-P_A_Givent;
        mean_classA=sum_class0/(double)CountClass0;
        mean_classB=sum_class1/(double)CountClass1;
        variance_A=Statistic::calcu_variance(class_0);
        variance_B=Statistic::calcu_variance(class_1);

    } else{
        P_A_Givent=CountClass1/(double)(CountClass0+CountClass1);
        P_B_Givent=1-P_A_Givent;
        mean_classB=sum_class0/(double)CountClass0;
        mean_classA=sum_class1/(double)CountClass1;
        variance_A=Statistic::calcu_variance(class_1);
        variance_B=Statistic::calcu_variance(class_0);
    }
//        a = s 2A − s 2B
//        b = 2(x ̄As2B − x ̄Bs2A)
//        c = (x ̄BsA)2 − (x ̄AsB)2 + 2s2As2B log[{p(A|t)sB}/{p(B|t)sA}].
    double a=variance_A-variance_B;
    double b=2*(mean_classA*variance_B-mean_classB*variance_A);
    double c=pow(mean_classB*sqrt(variance_A),2)-pow(mean_classA*sqrt(variance_B),2)+2*variance_A*variance_B*log((P_A_Givent*sqrt(variance_B))/(P_B_Givent*variance_A));
    double d=-1;//split point
    if(a==0){
        if(mean_classB==mean_classA){
            d=mean_classA;
        }else{
            d=(mean_classA+mean_classB)/2-(1/(mean_classA-mean_classB))*variance_A*log(P_A_Givent/P_B_Givent);
        }
    }else {
        if(b*b-4*a*c<0){
            d=(mean_classA+mean_classB)/2;
        } else{
            double root1=(-b-sqrt(b*b-4*a*c))/(2*a);
            double root2=(-b+sqrt(b*b-4*a*c))/(2*a);
            if(abs(root1-mean_classA)<abs(root2=mean_classA)){
                d=root1;
            } else{
                d=root2;
            }
            if(d>*max_element(class_0.begin(),class_0.end()) && d>*max_element(class_1.begin(),class_1.end()) && d<*min_element(class_0.begin(),class_0.end()) && d<*min_element(class_1.begin(),class_1.end()) ){
                d=(mean_classA+mean_classB)/2;
            }
        }
    }


    return d;
    //
}

Node* QuestDecisionTree::trainingTree(Table TrainingData,VarVector variables) {
    /*
     * Training method for the tree and sub tree
     * Table TrainingData   --Training data for training tree or sub tree.
     * variables            --Containing all the variables, may with different statues, used or not.
     * */
    Node* RootNode=new Node(TrainingData,variables);
    // If all the training samples are in one class,
    // then return a single root nod
    if(true){
        double tempLabel=TrainingData[0][(TrainingData[0].size()-1)];
        int count=0;
        for (int k=0;k<(TrainingData.size());k++){
            if(tempLabel==TrainingData[k][(TrainingData[k].size()-1)])
                count++;
        }
        if(count==TrainingData.size()){
            //  cout<<"All the remaining samples are in one class, temporary subtree Training complete!!"<<endl;
            RootNode->isLeaf=true;
            RootNode->classified=Statistic::determain_class(TrainingData);
            return RootNode;
        }

    }

    // if all the attributes have been used in the tree,return
    if(true){
        int usedVarCount=0;
        for(int k=0;k<variables.size();k++){
            if(variables[k].isUsed){
                usedVarCount++;
            }
        }
        if(usedVarCount==variables.size()) {
            // all the attributes has been used!
            //   cout<<"All the variables has been selected, temporary subtree returned, waiting for the other subtree."<<endl;
            RootNode->isLeaf=true;
            RootNode->classified=Statistic::determain_class(TrainingData);
            return RootNode;
        }
    }
//


    //variable selection
    Variable tempSelectedAttribute=variableSelection(TrainingData,RootNode->var_sel_sta);
    tempSelectedAttribute.isUsed=true;
    RootNode->var_sel_sta[tempSelectedAttribute.id].isUsed=true;

    // Table* Pdata=&TrainingData;
    double tempSplitPoint=splitSelection(TrainingData,tempSelectedAttribute);
    if(!(tempSplitPoint==tempSplitPoint)){
        RootNode->isLeaf=true;
        RootNode->classified=Statistic::determain_class(TrainingData);

        return RootNode;
    }

    RootNode->attribute=tempSelectedAttribute;
    RootNode->splitPoint=tempSplitPoint;

    variables[tempSelectedAttribute.type].isUsed=true;

    Table leftChildData;
    Table rightChildData;
    for(int k=0;k<TrainingData.size();k++){
        if(TrainingData[k][tempSelectedAttribute.id]<=tempSplitPoint){
            leftChildData.push_back(TrainingData[k]);
            //  RootNode.

        }else{
            rightChildData.push_back(TrainingData[k]);
        }
    }

    RootNode->leftChild=trainingTree(leftChildData,RootNode->var_sel_sta);
    RootNode->rightChild=trainingTree(rightChildData,RootNode->var_sel_sta);
    if(RootNode->leftChild==NULL && RootNode->rightChild==NULL){
        RootNode->isLeaf=true;
        RootNode->classified=Statistic::determain_class(TrainingData);
    }
    return RootNode;

}







Kmeans::Kmeans(int k, Table data){

    // clustereddata.push_back(new Record{2,3});
    Table clusters[2];
    RecordDouble means[2];

    for(int i=0;i<k;i++){
        //means[i]=new RecordDouble;
        RecordDouble tt;
        for(int j=0;j<5;j++){
            //  printf("ppppppppp\n");
            means[i].push_back((double)data[i][j]);
            //printf("ttttttttt\n");
        }
        // means[i]=tt;
    }

    int lable=0;

    for(int s=0;s<data.size();s++){
        lable=clusterOfTuple(means,data[s]);
        clusters[lable].push_back(data[s]);

    }

    double oldSquareError=-1;
    double newSquareError=getSquareError(clusters,means);
    while(abs(newSquareError-oldSquareError)>=1){
        for(int i=0;i<k;i++){
            means[i]=getMeans(clusters[i]);

        }
        oldSquareError=newSquareError;
        newSquareError=getSquareError(clusters,means);
        for(int j=0;j<k;j++){
            clusters[j].clear();
        }

        for(int s=0;s<data.size();s++){
            lable=clusterOfTuple(means,data[s]);
            clusters[lable].push_back(data[s]);

        }
    }


    for(double i=0;i<k;i++){
        Table tmp=clusters[(int)i];
        for(int j=0;j<tmp.size();j++){
            tmp[j].push_back(i);
            RecordDouble t=tmp[j];
            clustereddata.push_back(t);
        }
    }

}


double Kmeans::getDistance(RecordDouble tuple1,RecordDouble tuple2){
    /*
     * Calculate Euclid's distance.
     * */
    return sqrt(pow(tuple1[0]-tuple2[0],2)+pow(tuple1[1]-tuple2[1],2)+pow(tuple1[2]-tuple2[2],2)+pow(tuple1[3]-tuple2[3],2)+pow(tuple1[4]-tuple2[4],2));
}


int Kmeans::clusterOfTuple(RecordDouble means[],RecordDouble tuple){
    /*
     * Determaine the cluster of a tuple, according to the distance to center point.
     * */
    float dist=getDistance(means[0],tuple);
    float tmp;
    int label=0;//标示属于哪一个簇
    for(int i=1;i<2;i++){
        tmp=getDistance(means[i],tuple);
        if(tmp<dist) {
            dist=tmp;
            label=i;
        }
    }
    return label;
}

float Kmeans::getSquareError(Table clusters[],RecordDouble means[]){
    /*
     * Calculate square.
     *
     * */
    double error=0;
    for(int i=0;i<2;i++){
        Table tmp=clusters[i];
        for(int j=0;j<tmp.size();j++){
            error+=getDistance(means[i],tmp[j]);

        }
    }
    return error;
}

RecordDouble Kmeans::getMeans(Table cluster){
    /*
     * calculate center point of the cluster.
     * */
    double num = cluster.size();
    double means0 = 0, means1 = 0,means2=0,means3=0,means4=0;
    RecordDouble t;
    for (int i = 0; i < num; i++)
    {
        means0 += cluster[i][0];
        means1 += cluster[i][1];
        means2+=cluster[i][2];
        means3+=cluster[i][3];
        means4+=cluster[i][4];
    }
    t.push_back( means0 / num);
    t.push_back( means1 / num);
    t.push_back(means2/num);
    t.push_back(means3/num);
    t.push_back(means4/num);
    return t;
}
