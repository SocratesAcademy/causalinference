# 《基本有用的计量经济学》


 *关注公众号：causal-inference，了解更多信息

本仓库提供了MUSE中所使用的所有数据

## 数据
- nsw_dw.dta 是Dehejia and Wahba (1999) 使用的随机化实验 (NSW) 的数据。
- cps_controls.dta 是 Dehejia and Wahba (1999)所使用的美国当前人口调查(CPS)数据。
- chip2002.dta 是中国居民收入调查 (CHIP) 2002年城镇居民收入数据。
- angrist.dta(angrist.zip) 是 Angrist and Krueger (1991) 所使用的1980年美国人口普查数据5%的子样本。
- cardkrueger1994.dta 是 Card and Krueger (1994) 所使用的快餐店就业数据。
- smoking.dta 是 Abadie et al. (2010)合成控制法所使用的加州香烟销售数据。
- lee.dta 是 Lee(2008)所使用的美国众议员竞选数据，group_final.dta是原始数据。

## Stata代码
- programs.zip是第5章至第9章的主要程序，还是比较初步，是我去年上课时写的，后面会不定时更新，供读者参考。
### 补充程序
- rdrobust.zip（最新程序）和rdrobust_old.zip（老版本程序，可以画CV图）是断点回归程序，有些读者反映无法连接google下载，在此提供。解压后将所有文件拷贝到Stata目录\ado\plus\r或\ado\updates\r目录下即可使用。
- rddensity.zip 是与rdrobust配套的程序，可以进行McCrary(2008)的密度检验，也可以画出检验图形，若使用图形，还需要安装依赖程序包lpdensity.zip。rddensity.zip解压后将所有文件拷贝到\ado\plus\r或\ado\updates\r目录下，lpdensity.zip所有文件解压后拷贝到\ado\plus\l或\ado\updates\l目录下。
- McCray也提供了一个程序，DCdensity也可以进行McCrary(2008)密度检验并画图，从其网站
http://eml.berkeley.edu/~jmccrary/DCdensity/ 下载DCdensity.ado，并保存在\ado\plus\d目录下，即可使用。

### R代码

李建成老师将本书程序转化为R代码，有需要者可以参见[这里](https://mp.weixin.qq.com/s?__biz=MzI4NDcwMTU5MA==&mid=2247483846&idx=1&sn=b47acecf137e75aa6c262f75c1994be3&chksm=ebf6273fdc81ae29784914ad2e7d4eecf745599e9dabe1d1cf0ef1be761e1e270b09f6e030eb&mpshare=1&scene=1&srcid=0802fil7UjOOIAse9cWfjyz0&sharer_sharetime=1574208259547&sharer_shareid=ff2a7bc1fe1071d680581c5ea74f123f&key=ec7666f66bb2b36d3e3de8cfd67cd659e4aeff15ac913052e9c8021835c897d23c04be5a550f18923c5e151242e3ec2ae0b9021b36ab6f167d3a7e31252f797f5d8a9808e5341458907bb3bfcf27ef4b&ascene=1&uin=MTk2NTAyNjIxMw%3D%3D&devicetype=Windows+10&version=62070158&lang=zh_CN&pass_ticket=16Q3tRt5%2FxKNnIu5%2BSebLJx7G7LKQq%2FgP3kHYW%2FA4w3AA7EWN6couXb5rKSwC7Jt)。

## 勘误
- errata.docx是勘误表，是笔者和热心读者目前发现的印刷错误，为读者带来的不便深表歉意。如果读者发现其他错误可以发给：zhaoxiliang@gmail.com。多谢！

## PPT
PPT.zip 是上课用的课件。





