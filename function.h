//
// Created by tianle on 11/16/17.
//

#ifndef QUESTDECISIONTREE_FUNCTION_H
#define QUESTDECISIONTREE_FUNCTION_H

#include <iostream>
#include <fstream>
#include <iomanip>
#include <cstring>
#include <vector>
#include <map>
#include <cmath>
#include <algorithm>
#include <numeric>
#include <gsl/gsl_cdf.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_permutation.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_vector_double.h>


using namespace std;

// some type definitions
typedef vector<string> StringRecord;
typedef vector<StringRecord> StringTable;
typedef map<string,bool> MapStrBool;
typedef map<string,int> MapStrInt;
typedef vector<int> Record;

typedef vector<double> RecordDouble;
typedef vector<RecordDouble> Table;

class Variable{
    /* Variable class indicates the variables used to make decision tree and
     * some attributes of one variable.
     *
     * */
public:
    string name; //variable name
    int type;    // Variable type which consist of Ordered variable and categorical variable.
    int id;       // 1 and 2 denote Ordered variable and categorical variable respectively.
    bool isUsed;  // indicate if the variable has been used for the tree.
};


typedef vector<Variable> VarVector;



class Node{
    /*
     * Node class
     * */
public:
    Variable attribute;    //selected variable of the node
    VarVector var_sel_sta; //all the variables and it's status--used or not used.
    Table data_sta;        //Remaining training data of the node
    double splitPoint;     // split point of the node.
    bool isLeaf;           // leaf node or not.
    double classified;     // classified results for leaf node, otherwise,empty value.
    Node* leftChild;       // left child
    Node* rightChild;      // right child
    Node();
    Node(Table data_sta,VarVector var_sta);
};


class Statistic{
    /*
     * Some statistic method, which will be used.
     *
     * */
public:
//static RecordDouble FvalueWithAnovaTest(Table data,VarVector variables); // Analysis of variance, with F values returned. using square deviation
    //static RecordDouble FvalueAnovaTestWithAbs(Table data,VarVector variables);// Analysis of variance, with F values returned. using absolute deviation
    //static double ChiSquareTest(Table data,int column1,int column2); // Chi-square analysis.
    //static double calcu_variance(RecordDouble sets);  // Calculate variance
    //static double determain_class(Table data); // Determaine the class label according to the most frequent class in the dataset.

    static  RecordDouble FvalueWithAnovaTest(Table data,VarVector variables) {
        /*
         * Analysis of variance. with square deviation
         * data     --data
         * variables    ---Containing all the variables, may with different statues, used or not.
         *  return      -- the collection of F value for each variable
         * */

        RecordDouble Fvalue;
        for(int i=0;i<variables.size();i++){

            if (variables[i].type==1 && !(variables[i].isUsed)) {

                Table anovaTable;
                //统计该变量下Level的数目
                for(int m=0;m<data.size();m++){
                    //levels
                    RecordDouble tr;
                    tr.push_back(data[m][i]);
                    if(find(anovaTable.begin(),anovaTable.end(),tr)==anovaTable.end()){
                        anovaTable.push_back(tr);
                    }

                }
                sort(anovaTable.begin(),anovaTable.end());
//
//              将每一个level的观测值加入

                for(int p=0;p<anovaTable.size();p++){
                    for(int q=0;q<data.size();q++){
                        if(data[q][i]==anovaTable[p][0]){
                            anovaTable[p].push_back(data[q][data[q].size()-1]);
                        }
                    }
                }

                //计算每一个level的观测值的数量
                for(int j=0;j<anovaTable.size();j++){
                    anovaTable[j].push_back(accumulate(anovaTable[j].begin()+1,anovaTable[j].end(),0));
                    anovaTable[j].push_back(anovaTable[j].size()-2);
                }

                //利用anovatable 计算 F value
                double Sa=0;
                double interval=0;
                double T=0;
                double N=0;
                for(int k=0;k<anovaTable.size();k++){
                    interval+=pow(anovaTable[k][anovaTable[k].size()-2],2)/double (anovaTable[k][anovaTable[k].size()-1]);
                    T+=anovaTable[k][anovaTable[k].size()-2];
                    N+=anovaTable[k][anovaTable[k].size()-1];
                }
                Sa=interval-pow(T,2)/N;
                double MSa= Sa /double (anovaTable.size()-1);


                double interval2=0;

                for(int s=0;s<anovaTable.size();s++){
                    for(int t=1;t<anovaTable[s].size()-2;t++){
                        interval2+=pow(anovaTable[s][t],2);
                    }
                }

                double Se=interval2-interval;
                double MSe= Se / double (N-(anovaTable.size()));

                double F=MSa/MSe;
                if(!(F==F)){
                    F=-1;
                }
                Fvalue.push_back(F);
            }else {
                Fvalue.push_back(-1);
            }
        }
        return Fvalue;
    }

  static    RecordDouble FvalueAnovaTestWithAbs(Table data,VarVector variables){
        /*
         *
         * Analysis of variance. with abslolute deviation
         * data     --data
         * variables    ---Containing all the variables, may with different statues, used or not.
         *  return      -- the collection of F value for each variable
         *
         * */
        RecordDouble Fvalue;
        for(int i=0;i<variables.size();i++){

            if (variables[i].type==1 && !(variables[i].isUsed)) {

                Table anovaTable;
                //统计该变量下Level的数目
                for(int m=0;m<data.size();m++){
                    //levels
                    RecordDouble tr;
                    tr.push_back(data[m][i]);
                    if(find(anovaTable.begin(),anovaTable.end(),tr)==anovaTable.end()){
                        anovaTable.push_back(tr);
                    }
                }
                sort(anovaTable.begin(),anovaTable.end());

                for(int p=0;p<anovaTable.size();p++){
                    for(int q=0;q<data.size();q++){
                        if(data[q][i]==anovaTable[p][0]){
                            anovaTable[p].push_back(data[q][data[q].size()-1]);
                        }
                    }
                }

                //计算每一个level的观测值的数量
                for(int j=0;j<anovaTable.size();j++){
                    anovaTable[j].push_back(accumulate(anovaTable[j].begin()+1,anovaTable[j].end(),0));
                    anovaTable[j].push_back(anovaTable[j].size()-2);
                }

                //利用anovatable 计算 F value
                double Sa=0;
                double interval=0;
                double T=0;
                double N=0;
                for(int k=0;k<anovaTable.size();k++){
                    interval+=abs(anovaTable[k][anovaTable[k].size()-2])/double (anovaTable[k][anovaTable[k].size()-1]);
                    T+=anovaTable[k][anovaTable[k].size()-2];
                    N+=anovaTable[k][anovaTable[k].size()-1];
                }
                Sa=interval-abs(T)/N;
                double MSa= Sa /double (anovaTable.size()-1);


                double interval2=0;

                for(int s=0;s<anovaTable.size();s++){
                    for(int t=1;t<anovaTable[s].size()-2;t++){
                        interval2+=abs(anovaTable[s][t]);
                    }
                }

                double Se=interval2-interval;
                double MSe= Se / double (N-(anovaTable.size()));

                double F=MSa/MSe;
                Fvalue.push_back(F);
            }else {
                Fvalue.push_back(-1);
            }
        }

        return Fvalue;
    }

    static double ChiSquareTest(Table data,int column1,int column2) {
        /*
         * Chi-square test using two column vector
         * data     --data
         * column1  --the index of first column
         * column2  --the index of second column
         * return   --the P value of chi-square test.
         *
         * */
        Table contigencytable;

        RecordDouble columnvector1;
        RecordDouble columnvector2;

        for(int i=0;i<data.size();i++){
            if(find(columnvector1.begin(),columnvector1.end(),data[i][column1])==columnvector1.end()){
                columnvector1.push_back(data[i][column1]);
            }
        }
        sort(columnvector1.begin(),columnvector1.end());


        RecordDouble lastline;
        for(int t=0;t<columnvector1.size()+1;t++){
            lastline.push_back(0);
        }
        for(int m=1;m<=3;m++) {
            RecordDouble line;

            for (int n = 0; n < columnvector1.size(); n++) {
                int counter=0;   // the number cases that column1=n && column2=m
                for (int k = 0; k < data.size(); k++) {
                    if (data[k][column1] == columnvector1[n] && data[k][column2] == m) {
                        counter++;
                    }
                }
                line.push_back((double)counter);
            }
            double sum=accumulate(line.begin(),line.end(),0);
            line.push_back(sum);
            contigencytable.push_back(line);
            for(int s=0;s<line.size();s++){
                lastline[s]+=line[s];
            }

        }
        contigencytable.push_back(lastline);
        double chisquare=0;

        int s=contigencytable.size();
        int r=contigencytable[0].size();
        for(int i=0;i<s;i++){
            for(int j=0;j<r;j++){
                chisquare+=pow(contigencytable[s-1][r-1]*contigencytable[i][j]-contigencytable[i][r-1]*contigencytable[s-1][j],2)/double (contigencytable[s-1][r-1]*contigencytable[i][r-1]*contigencytable[s-1][j]);
            }
        }
        double mu=(s-1)*(r-1);
        double pValue=gsl_cdf_chisq_P(chisquare,mu);
        return pValue;

    }

    static double calcu_variance(RecordDouble resultSet){
        /*
         * Calculate variance of one list
         * */
        double sum = std::accumulate(std::begin(resultSet), std::end(resultSet), 0.0);
        double mean =  sum / resultSet.size(); //均值

        double accum  = 0.0;
        std::for_each (std::begin(resultSet), std::end(resultSet), [&](const double d) {
            accum  += (d-mean)*(d-mean);
        });

        double var = accum/(double)(resultSet.size()); //方差
        return var;
    }

    static double determain_class(Table data) {
        /*
         * Determaine the class label according to the most frequest class of the dataset.
         *
         * */
        double class1=0;
        double class2=0;
        double class3=0;
        for(int i=0;i<data.size();i++){
            if(data[i][5]==1){
                class1++;
            }else if(data[i][5]==2){
                class2++;
            }else{
                class3++;
            }
        }
        double maxclass=1;
        if(class2>class1 && class2>class3){
            maxclass=2;
        }
        if(class3>class1 && class3>class2){
            maxclass=3;
        }

        if(class1>class2 && class1>class3){
            maxclass=1;
        }
        return maxclass;
    }


};

class MatrixOp{
    /*
     * some matrix operations.
     * */
public:
    MatrixOp();
//    static void gsl_matrix_mul(gsl_matrix *a,gsl_matrix *b,gsl_matrix *c);
//    static void gsl_matrix_inverse(gsl_matrix *A, gsl_matrix *inverse);
//    static gsl_matrix* gsl_matrix_transpose(gsl_matrix *A);

    float getSquareError(Table clusters[],RecordDouble means[]);
    RecordDouble getMeans(Table cluster);


    static void gsl_matrix_mul(gsl_matrix *a,gsl_matrix *b,gsl_matrix *c)
    {
        /*
         * Matrix multiplication,the data structure of matrix must be gsl_matrix.
         * a    -- Matrix A
         * b    -- matrix B
         * c    -- matrix of AB
         * */
        for (size_t i=0;i<a->size1;i++)
        {
            for (size_t j=0;j<b->size2;j++)
            {
                double sum=0.0;
                for (size_t k=0;k<b->size1;k++)
                {
                    sum+=gsl_matrix_get(a,i,k)*gsl_matrix_get(b,k,j);
                }
                gsl_matrix_set(c,i,j,sum);
            }
        }
    }

    static void gsl_matrix_inverse(gsl_matrix *A, gsl_matrix *inverse)
    {
        /*
         * Matrix inverse operation, the data structure of matrix must be gsl_matrix.
         * A        --matrix
         * inverse  --inverse matrix of A
         * */
        int n = A->size1;
        gsl_matrix *tmpA = gsl_matrix_alloc(n, n);
        gsl_matrix_memcpy(tmpA, A);
        gsl_permutation *p = gsl_permutation_alloc(n);
        int sign = 0;
        gsl_linalg_LU_decomp(tmpA, p, &sign);
        inverse = gsl_matrix_alloc(n, n);
        gsl_linalg_LU_invert(tmpA, p, inverse);
        gsl_permutation_free(p);
        gsl_matrix_free(tmpA);
    }

    static gsl_matrix* gsl_matrix_transpose(gsl_matrix *A){
        /*
         * Matrix transpose operation, the data structure of matrix must be gsl_matrix.
         * A        --Matrix A
         * return   --the transpose of A
         * */
        gsl_matrix *B=gsl_matrix_alloc(A->size2,A->size1);

        for(int i=0;i<A->size2;i++){
            for(int j=0;j<A->size1;j++){
                gsl_matrix_set(B,i,j,gsl_matrix_get(A,j,i));
            }
        }
        return B;
    }

};



class Kmeans {
public:
    /* Kmeans clustering impletation, which will be used later.
     *
     * */
    Table clustereddata;

    Kmeans();
    Kmeans(int k, Table data);
    double getDistance(RecordDouble tuple1,RecordDouble tuple2);
    int clusterOfTuple(RecordDouble means[],RecordDouble tuple);
    float getSquareError(Table clusters[],RecordDouble means[]);
    RecordDouble getMeans(Table cluster);
};


class QuestDecisionTree{
    /*
     * The construction of decision tree.
     * */
private:
    Node* Tree;    //Decision tree
    int VariableNum; // the number of all the variables
    Table TrainingData; //Training data
    VarVector variables;// variable set.

public:
    QuestDecisionTree();
    QuestDecisionTree(Table data,VarVector variables);
    Node* trainingTree(Table TrainingData,VarVector variables);
    Variable variableSelection(Table data,VarVector variables);
    double splitSelection(Table& data,Variable splitVar);

    double SplitSelection_Ordered(Table data,Variable splitVar);

    Node* get_tree();
};

#endif //QUESTDECISIONTREE_FUNCTION_H
