#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <cmath>
#include <fstream>
#include <sstream>
#include <algorithm>
using namespace std;
#define MaxLength 5


class Node {
public:
	string attribute_spilt;
	string spilt_value;
	vector<Node *> child;
	Node() {
		spilt_value = "";
		attribute_spilt = "";
	}
};

void Input(vector<string> &attribute, vector<vector<string>> &data)
{
	string s;
	vector<string> item;
	ifstream in;
	int j;
	in.open("weather.arff", fstream::in);
	for (int i = 0; i < 9; i++)
	{
		string b;
		char str[256];
		in.getline(str, sizeof(str), '\n');
		if (i > 1 && i<7)
		{
			for (j = 0; str[j] != ' '; j++);
			j++;
			for (; str[j] != ' '; j++)
			{
				b.push_back(str[j]);
			}
			attribute.push_back(b);
		}
	}
	data.push_back(attribute);
	attribute.erase(attribute.end() - 1, attribute.end());
	while (!in.eof())
	{
		char str1[256];
		string h, m;
		in.getline(str1, sizeof(str1), '\n');
		for (int i = 0; str1[i] != '\0'; i++)
		{
			if (str1[i] != ',')
				h.push_back(str1[i]);
			else
				h.push_back(' ');
		}
		h.push_back(' ');
		for (char c : h)
		{
			if ((c != ' '))
			{
				m.push_back(c);
			}
			else
			{
				item.push_back(m);
				m = "";
			}
		}
		data.push_back(item);
		item.clear();
	}

	return;
}

void Map(map<string, vector<string>> &attribute_value, vector<vector<string>> data)
{
	bool flag = true;
	int i, j, k;
	vector<string> value;
	for (i = 0; i<MaxLength; i++)
	{
		for (unsigned j = 1; j<data.size(); j++)
		{
			for (unsigned k = 0; k<value.size(); k++)
			{
				if (!(value[k].compare(data[j][i])))
				{
					flag = false;
					break;
				}
			}
			if (flag)
			{
				value.push_back(data[j][i]);
			}
			flag = true;
		}
		attribute_value[data[0][i]] = value;
		value.erase(value.begin(), value.end());
	}
}
//计算连续属性的信息增益率
double continuous_compute(int &flag,double Info_d, string attribute, vector<vector<string>> data)
{

	vector<string> item;
	vector<int> val;
	bool f=true;
	double max = 0.0,info,info_1,info_2,p1,p2,p3,p4;
	double con_info,spilt_info;//每个分裂点的信息增益率
	stringstream ss;
	vector<int> count_1(2,0);
	vector<int> count_2(2, 0);
	int k;
	for (k = 0; k < MaxLength; k++)
	{
		if (data[0][k] == attribute)
			break;
	}
	for (int i = 1; i < data.size (); i++)
	{
		for (int m = 0; m < item.size(); m++)
		{
			if (item[m] == data[i][k])
			{
				f = false;
				break;
			}
		}
		if (f)
		{
			item.push_back(data[i][k]);
		}
		f = true;
	}

	for (string s : item)
	{
		int a;
		ss << s;
		ss >> a;
		val.push_back(a);
		ss.clear();
	}
	sort(val.begin(), val.end());
	for (k = 0; k < data.size(); k++)
	{
		if (data[0][k] == attribute)
			break;
	}
	for (int b=0;b<val.size()-1;b++)
	{
		for (int j = 1; j < data.size(); j++)
		{
			ss << data[j][k];
			int c;
			ss >> c;
			ss.clear();
			if (c <= val[b] && data[j][MaxLength - 1] == "yes")
				count_1[0]++;
			if (c <= val[b] && data[j][MaxLength - 1] == "no")
				count_1[1]++;
			if (c > val[b] && data[j][MaxLength - 1] == "yes")
				count_2[0]++;
			if (c > val[b] && data[j][MaxLength - 1] == "no")
				count_2[1]++;
		}
		p1 = (double)count_1[0] / (count_1[0] + count_1[1]);
		p2 = (double)count_1[1] / (count_1[0] + count_1[1]);
		p3 = (double)count_2[0] / (count_2[0] + count_2[1]);
		p4 = (double)count_2[1] / (count_2[0] + count_2[1]);
		info_1 = (double)(count_1[0] + count_1[1]) / (data.size() - 1)*(-p1*(log(p1) / log(2.0)) - p2*(log(p2) / log(2.0)));
		info_2 = (double)(count_2[0] + count_2[1]) / (data.size() - 1)*(-p3*(log(p3) / log(2.0)) - p4*(log(p4) / log(2.0)));
		info = (double)Info_d - (info_1 + info_2);
		double a = (double)(count_1[0] + count_1[1]) / (data.size() - 1);
		double e = (double)(count_2[0] + count_2[1]) / (data.size() - 1);
		spilt_info = -a*(double)(log(a) / log(2.0)) - e*(double)(log(e) / log(2.0));
		con_info = (double)info / spilt_info;
		if (con_info >= max)
		{
			max = con_info;
			flag = val[b];     ////flag为分裂点
			//////////////更新连续属性的值为true或false
		}
	}
	return max;


}

double Compute_info_di(double Info_d,string attribute, map<string, vector<string>> attribute_value, vector<vector<string>> data)
{
	vector<string> val = attribute_value[attribute];
	int k;
	double Info_di = 0.0;
	double p1, p2,s;
	double info, spilt_info=0,gain_ratio;
	vector<int> count(2, 0);
	for (k = 0; k<MaxLength; k++)
	{
		if (data[0][k] == attribute)
		{
			break;
		}
	}
	for (unsigned int i = 0; i<val.size(); i++)
	{
		count[0] = 0;
		count[1] = 0;
		for (unsigned int j = 1; j<data.size(); j++)
		{
			if (data[j][k] == val[i] && data[j][MaxLength - 1] == "yes")
			{
				count[0]++;
				//cout<<data[j][k]<<"对应"<<val[i]<<"对应"<<data[j][MaxLength-1]<<endl;/////////////
			}
			if (data[j][k] == val[i] && data[j][MaxLength - 1] == "no")
			{
				count[1]++;
				//cout<<data[j][k]<<"对应"<<val[i]<<"对应"<<data[j][MaxLength-1]<<endl;/////////////
			}

		}
		//cout<<count[0]<<"and"<<count[1]<<endl;
		if (count[0] == 0 || count[1] == 0)
		{
			continue;
		}
		p1 = (double)count[0] / (count[0] + count[1]);
		p2 = (double)count[1] / (count[0] + count[1]);
		s = (double)(count[0] + count[1]) / data.size();
		spilt_info = spilt_info +(-s*(double)(log(s) / log(2.0)));

		double t = (-p1*(log(p1) / log(2.0)) - (p2*(log(p2) / log(2.0))));
		//cout<<t<<" "<<count[0]<<count[1]<<endl;///////////
		Info_di = Info_di + (double)(count[0] + count[1]) / (data.size() - 1)*t;
		//cout << Info_di << " " << count[0] + count[1] << endl;
	}
	info = (double)Info_d - Info_di;
	gain_ratio = (double)info / spilt_info;
	return gain_ratio;
}

string mostLabel(vector<vector<string>> data)
{
	unsigned int i;
	vector<int> count(2, 0);
	for (i = 1; i<data.size(); i++)
	{
		if (data[i][MaxLength - 1] == "yes")
		{
			count[0]++;
		}
		else
		{
			count[1]++;
		}
	}
	if (count[0]>count[1])
		return "yes";
	else
		return "no";
}
void Create_deciding_tree(Node *p, vector<string> attribute, vector<vector<string>> data, map<string, vector<string>> attribute_value)
{	//判断三个结束条件
	//判断所有训练集的元组是否属于同一个类
	vector<int> flag(2, 0);     //保存两个连续属性的分裂点
	vector<int> count(2, 0);
	for (unsigned int i = 1; i<data.size(); i++)
	{
		if (data[i][MaxLength - 1] == "yes")
		{
			count[0]++;
		}
		else
		{
			count[1]++;
		}
	}
	if (count[0] == data.size() - 1 || count[1] == data.size() - 1)
	{
		if (count[0] == data.size() - 1)
			p->attribute_spilt = "yes";
		else
			p->attribute_spilt = "no";
		return;
	}

	//所有的属性都已经分尽
	if (attribute.size() == 0)
	{
		p->attribute_spilt = mostLabel(data);
		return;
	}
	//确定划分前的所有元组属性信息增益
	double p1 = (double)count[0] / (data.size() - 1);
	double p2 = (double)count[1] / (data.size() - 1);
	double Info_d;
	if (count[0] == 0 || count[1] == 0)
		Info_d = 0;
	else
		Info_d = -p1*(log(p1) / log(2.0)) - p2*(log(p2) / log(2.0));

	//计算各个属性作为划分属性时的信息增益
	double max = 0;
	double Info_di;//每个属性作为分裂点时的信息增益率
	string spilt;
	for (unsigned int i = 0; i<attribute.size(); i++)
	{
		if ((attribute[i] == "temperature") || (attribute[i] == "humidity"))
		{
			if (attribute[i] == "temperature")
				Info_di = continuous_compute(flag[0],Info_d, attribute[i], data);
			else
				Info_di = continuous_compute(flag[1], Info_d, attribute[i], data);
		}
		else
		{
			Info_di = Compute_info_di(Info_d, attribute[i], attribute_value, data);
		}
		//Info=Info_d-Info_di;
		if (max<=Info_di)
		{
			max = Info_di;
			spilt = attribute[i]; //找到分裂属性
		}

	}
	//cout << endl;////////////////

	p->attribute_spilt = spilt;
	//开始增加孩子节点,进行划分
	//第一步：更新attribute属性数组，去除分裂属性
	vector<string> copy;
	for (unsigned int i = 0; i<attribute.size(); i++)
	{
		if (attribute[i] != spilt)
		{
			copy.push_back(attribute[i]);
		}
	}
	attribute.erase(attribute.begin(), attribute.end());
	attribute = copy;
	//为每个分区增加孩子节点
	vector<string> val;
	vector<vector<string>> new_data;
	vector<vector<string>> new_data1;
	new_data.push_back(data[0]);
	new_data1.push_back(data[0]);
	int k;
	if (spilt == "temperature" ||spilt == "humidity")
	{
		if (spilt == "temperature")
		{
			for (k = 0; k<MaxLength; k++)
			{
				if (data[0][k] == spilt)
				{
					break;
				}
			}
				Node* new_node1 = new Node();
				Node* new_node2 = new Node();

				for (unsigned int j = 1; j<data.size(); j++)
				{
					stringstream ss;
					ss << data[j][k];
					int c;
					ss >> c;
					if (c<=flag[0])
					{
						new_data.push_back(data[j]);
					}
					else
					{
						new_data1.push_back(data[j]);
					}
				}
				new_node1->spilt_value = "high";
				new_node2->spilt_value = "normal";
				p->child.push_back(new_node1);
				p->child.push_back(new_node2);
				if (new_data.size() == 1&& new_data1.size() != 1)
				{
					new_node1->attribute_spilt = mostLabel(data);
					Create_deciding_tree(new_node2, attribute, new_data1, attribute_value);
					new_data.erase(new_data.begin() + 1, new_data.end());
					new_data1.erase(new_data1.begin() + 1, new_data1.end());
					return;
				}
				if (new_data1.size() == 1&& new_data.size() != 1)
				{
					new_node2->attribute_spilt = mostLabel(data);
					Create_deciding_tree(new_node1, attribute, new_data, attribute_value);
					new_data.erase(new_data.begin() + 1, new_data.end());
					new_data1.erase(new_data1.begin() + 1, new_data1.end());
					return;
				}
				Create_deciding_tree(new_node2, attribute, new_data1, attribute_value);
				Create_deciding_tree(new_node1, attribute, new_data, attribute_value);
				new_data.erase(new_data.begin() + 1, new_data.end());
				new_data1.erase(new_data1.begin() + 1, new_data1.end());
				return;		
		}
		else
		{
			for (k = 0; k<MaxLength; k++)
			{
				if (data[0][k] == spilt)
				{
					break;
				}
			}
			Node* new_node1 = new Node();
			Node* new_node2 = new Node();

			for (unsigned int j = 1; j<data.size(); j++)
			{
				stringstream ss;
				ss << data[j][k];
				int c;
				ss >> c;
				if (c <= flag[1])
				{
					new_data.push_back(data[j]);
				}
				else
				{
					new_data1.push_back(data[j]);
				}
			}
			new_node1->spilt_value = "high";
			new_node2->spilt_value = "normal";
			p->child.push_back(new_node1);
			p->child.push_back(new_node2);
			if (new_data.size() == 1 && new_data1.size() != 1)
			{
				new_node1->attribute_spilt = mostLabel(data);
				Create_deciding_tree(new_node2, attribute, new_data1, attribute_value);
				new_data.erase(new_data.begin() + 1, new_data.end());
				new_data1.erase(new_data1.begin() + 1, new_data1.end());
				return;
			}
			if (new_data1.size() == 1 && new_data.size() != 1)
			{
				new_node2->attribute_spilt = mostLabel(data);
				Create_deciding_tree(new_node1, attribute, new_data, attribute_value);
				new_data.erase(new_data.begin() + 1, new_data.end());
				new_data1.erase(new_data1.begin() + 1, new_data1.end());
				return;
			}
			Create_deciding_tree(new_node2, attribute, new_data1, attribute_value);
			Create_deciding_tree(new_node1, attribute, new_data, attribute_value);
			new_data.erase(new_data.begin() + 1, new_data.end());
			new_data1.erase(new_data1.begin() + 1, new_data1.end());
			return;
		}
	}
	val = attribute_value[spilt];

	for (k = 0; k<MaxLength; k++)
	{
		if (data[0][k] == spilt)
		{
			break;
		}
	}
	for (unsigned int i = 0; i<val.size(); i++)
	{
		Node* new_node = new Node();
		for (unsigned int j = 1; j<data.size(); j++)
		{
			if (data[j][k] == val[i])
			{
				new_data.push_back(data[j]);
			}
		}
		//若分区为空，则子树为叶子结点标识为元组数最多的类
		new_node->spilt_value = val[i];
		if (new_data.size() == 1)
		{
			new_node->attribute_spilt = mostLabel(data);
			return;
		}
		//创建子树
		Create_deciding_tree(new_node, attribute, new_data, attribute_value);
		p->child.push_back(new_node);
		new_data.erase(new_data.begin() + 1, new_data.end());
	}
	return;
}


void Print_tree(Node *p,int depth)
{
	for (int i = 0; i < depth; i++)
		cout << "\t";
	if (p->spilt_value != "")
	{
		cout << p->spilt_value << endl;
		for (int i = 0; i < depth + 1; i++)
		{
			cout << "\t";
		}
	}
	cout << p->attribute_spilt << endl;
	if (p->attribute_spilt == "yes" || p->attribute_spilt == "no")
		return;
	for (int i = 0; i<(p->child).size(); i++)
	{
		Print_tree((p->child)[i],depth+1);
	}

}


void main()
{
	vector<string> attribute;
	vector<vector<string>> data;
	map<string, vector<string>> attribute_value;
	Input(attribute, data);
	Map(attribute_value, data);
	Node* root = new Node();
	Create_deciding_tree(root, attribute, data, attribute_value);
	cout << "The tree is:" << endl;
	Print_tree(root,0);
	system("pause");
	return;
}