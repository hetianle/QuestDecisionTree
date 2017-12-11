#include "function.h"
#include <zconf.h>
#include <stack>
#include <time.h>


Table t;
void print_tree(Node* tree);

void get_test(Node* tree);

void decision_rules(Node* tree,int leftorright);

double classfiy(RecordDouble,Node* tree);

string doubleToString(double num);

//    ./DT -r training.txt -t testing.txt -d 65 -c 2 -s 32768 -m 2000 –p 0 …其它参数

int main(int argc,const char *argv[]) {
    clock_t start,finish;
    double totaltime;
    start=clock();


    char buff[1000];
    getcwd(buff, 1000);
  //  std::cout << "当前路径是："<<buff<<"\n";
    string tempdir=buff;
   // getchar();


    string file1=tempdir+"/"+argv[1]; //training file
    string file2=tempdir+"/"+argv[2]; // meta data
    string file3=tempdir+"/"+argv[3]; // test file
   // cout<<file1<<endl<<file2<<endl<<file3<<endl;
    fstream fs;


    fs.open(file1,ios_base::in);
    fstream fs2;
    fs2.open(file2,ios_base::in);
    fstream fs3;
    fs3.open(file3,ios_base::in);

   // fs.open();
    if(!fs || !fs2){
        printf("读取文件为空！\n");
        return 0;
    }
    char a[100];

    //读取训练数据
    vector<RecordDouble> data;
    while (fs.getline(a,100)){
        RecordDouble line;
        string s{a};
        for(int i=0;i<6;i++) {
            string q = s.substr(0, s.find(','));
            line.push_back(stoi(q));
            s=s.substr(s.find(',')+1,s.length());
        }
        data.push_back(line);
    }
    fs.close();




    map<string,int> mapVarType;
  //  mapVarType.insert(make_pair("binary",0));
    mapVarType.insert(make_pair("ordered",1));
    mapVarType.insert(make_pair("categorical",0));

    //读取元数据文件

    VarVector metadata;
    int i=0;
    while (fs2.getline(a,100)){
        Variable row;
        string r{a};
        row.name=r.substr(0,r.find(','));
        row.type=mapVarType.find(r.substr(r.find(',')+1,r.length()))->second;
        row.isUsed=false;
        row.id=i++;
        metadata.push_back(row);

    }
    fs2.close();


    //读取测试数据

    if(!fs3){
        printf("test.dat 读取文件为空！\n");
        return 0;
    }
    char b[100];
    //Table data;
    vector<RecordDouble> test_data;
    while (fs3.getline(b,100)){
        RecordDouble line;
        string s{b};
        for(int i=0;s.length();i++) {
            string q = s.substr(0, s.find(','));
            line.push_back(stod(q));
            s=s.substr(s.find(',')+1,s.length());
        }
        test_data.push_back(line);
    }
    fs3.close();
    //开始训练决策树
    cout<<"\n训练开始：\n\n";
    QuestDecisionTree* tree=new QuestDecisionTree(data,metadata);

    //训练完成，打印出决策树
    Node* root=tree->get_tree();
    cout<<"训练完成\n";
    cout<<"深度优先检索训练好的决策树，并打印出每一个节点：\n注：对于确定性属性，Quest决策树首先需要将确定性属性转化为顺序属性，故训练好的决策树节点只保留了转换后的分裂值。"<<endl;
    print_tree(root);
    cout<<"\n\n";
    cout<<"测试开始：\n";

    //输入测试数据，并打印出测试结果
    double test_num=test_data.size();
    double correct=0;



    Table confusion;
    for(int i=0;i<3;i++){
        RecordDouble row;
        for(int j=0;j<3;j++){
            double z=0;
            row.push_back(z);

        }
        confusion.push_back(row);
    }


    for(int i=0;i<test_data.size();i++){
        double pre=classfiy(test_data[i],root);
        double lab=test_data[i][test_data[i].size()-1];

        if(lab==1 && pre==1){
            confusion[0][0]++;
        }else if(lab==1 && pre==2)
            confusion[0][1]++;
        else if(lab==1 && pre ==3)
            confusion[0][2]++;
        else if(lab==2 && pre==1)
            confusion[1][0]++;
        else if(lab==2 && pre==2)
            confusion[1][1]++;
        else if(lab==2 && pre==3 )
            confusion[1][2]++;
        else if(lab==3 && pre==1)
            confusion[2][0]++;
        else if(lab==3 && pre==2)
            confusion[2][1]++;
        else if(lab==3 && pre==3)
            confusion[2][2]++;


        if(pre==lab){
            correct++;
        }
        cout<<"sample"<<i+1<<":predict "<<pre<<",truelabel "<<lab<<endl;
    }
    cout<<"=======================================\n precision:"<<correct/test_num<<endl;

    cout<<"\n\n 3类 confusion matrix:\n";
    for(int i=0;i<confusion.size();i++){
        for(int j=0;j<confusion[i].size();j++){
            cout<<"\t"<<confusion[i][j]<<"\t";
        }
        cout<<"\n";
    }
//
//    get_test(root);
//    Table *n=&t;


    finish=clock();
    totaltime=(double)(finish-start)/CLOCKS_PER_SEC;
    cout<<"\n此程序的运行时间为"<<totaltime<<"秒！"<<endl;
    return 0;
}


void get_test(Node* tree){

    //Table t;
    if(tree->isLeaf==true){
        //cout<<"叶子节点，该分支分类结果：："<<tree->classified<<endl;
        t.insert(t.end(),tree->data_sta.begin(),tree->data_sta.end());
        return;
    }
    get_test(tree->leftChild);
    get_test(tree->rightChild);
}

void print_tree(Node* tree){
//   深度优先遍历输出决策树
    if(tree->isLeaf==true){
        cout<<"叶子节点，该分支分类结果：："<<tree->classified<<endl;
        return;
    }
    cout<<"\n"<<"分类属性："<<tree->attribute.name<<"分裂点："<<tree->splitPoint<<endl;
    cout<<"--------------------------------------------------\n";

    print_tree(tree->leftChild);
    print_tree(tree->rightChild);

}

double classfiy(RecordDouble tuple,Node* root){

    double pre;
    if(!root->isLeaf) {
        if (tuple[root->attribute.id]<root->splitPoint){
            pre=classfiy(tuple,root->leftChild);
        } else{
            pre=classfiy(tuple,root->rightChild);
        }
    } else{
        return root->classified;
    }
    return pre;
}


vector<string> vector_str;
void decision_rules(Node* tree,int leftorright) {
    //深度优先遍历输出决策树

    if(tree->isLeaf){
        vector_str.push_back("分类结果"+doubleToString(tree->classified));
        //cout<<"叶子节点，该分支分类结果：："<<tree->classified<<endl;
        string s;
        for(int i=0;i<vector_str.size();i++){
            s+=vector_str[i];

        }
         cout<<s<<"\n"<<endl;
        vector_str.pop_back();
        return;
    }
//    cout<<"\n"<<"分类属性："<<tree->attribute.name<<"分裂点："<<tree->splitPoint<<endl;
//    cout<<"--------------------------------------------------\n";
    string r;
    if(leftorright<1) {


        r = tree->attribute.name + " < " + doubleToString(tree->splitPoint) + " AND\n";
    } else{
        r = tree->attribute.name + " > " + doubleToString(tree->splitPoint) + " AND\n";
    }

    vector_str.push_back(r);
    decision_rules(tree->leftChild,0);
    decision_rules(tree->rightChild,1);


//    vector<string> rules;
//    stack<string> stack_str;
//    string r;
//    while (!(tree->isLeaf)) {
//        r=tree->attribute.name + "<" + doubleToString(tree->splitPoint)+ " AND\n";
//        stack_str.push(r);
//        tree=tree->leftChild;
//        if(tree->isLeaf){
//            r+="clssified:"+doubleToString(tree->classified);
//        }
//    }
//    cout<<r<<endl;


}

string doubleToString(double num)
{
    char str[256];
    sprintf(str, "%lf", num);
    string result = str;
    return result;
}