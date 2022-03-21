# ColaResearch
## 研究目标
用R语言以及panel data包（面板数据模型）， 基于营销组合模型分析往期销售收入、价格、广告、促销等因素是如何影响可乐公司销售收入的， 并计算不同广告类型的成本效益率以及对销售收入的贡献值。

## 研究假设
假设1: 基于Avena的研究（2008）表明，糖瘾和咖啡因依赖是可乐消费者的重要特征，这会导致重复购买。<br>假设2: 总评分点对销售有直接和积极的影响。

## 数据预处理
使用log方法处理异常值，从而更好地观察数据特征，提升模型预测能力。
![image](https://user-images.githubusercontent.com/79248301/159374492-97b99207-1444-4c7b-96eb-50ead9531553.png)

## 建模
1. 使用面板数据模型对原数据进行处理；
2. 通过对比分析线性模型、log-linear模型、log-log模型，根据R-squared值，发现log-log模型最佳；
![image](https://user-images.githubusercontent.com/79248301/159375623-c9d7fcf5-7d65-4b21-9bc5-6959c91a40e2.png)
3. 根据营销组合模型(marketing mix model)，考虑广告的延迟累计影响，并将其作为变量加入模型中；
<img width="845" alt="image" src="https://user-images.githubusercontent.com/79248301/159375360-848024f0-7883-4b67-8765-4048fa667599.png">
4. 根据动态面板数据模型，考虑往期销售收入对本期销售收入的影响，并将其作为变量加入模型中；<br>
5. 模型测试：Hausman Test、Time Fixed Effect Test、Cross-sectional Dependence & Serial Correlation Test、VIF Test、Homoskedasticity Test、Robust Test。

## 数据可视化和预测
![image](https://user-images.githubusercontent.com/79248301/159375682-855eee35-9361-4d7e-8695-1a43112018e8.png)

## 结论
价格变化、促销活动和广告占总销售额的53%，约为3.99亿。其中：5900万与电视广告有关，1.27亿与横幅广告有关，3.95亿与促销活动有关。<br>
此外，消费者对价格极为敏感。促销活动是对销售影响第二大的变量，这也与消费者的价格敏感性有关。53%的当月销售受到上月销售的影响，因此，一半以上在上月购买可乐的消费者会再次购买该产品（未能拒绝假设1）。横幅广告（32%）对销售的影响比电视广告（15%）更大，因此在总评分点较低的情况下，横幅广告的顾客转化率更高（拒绝假设2）。已知电视广告成本为200万英镑/年，横幅广告成本为50万英镑/年，每单位电视和横幅广告成本产生的相关收入分别为17英镑和148英镑，因此横幅广告的成本效益更高。
