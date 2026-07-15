# 更新日志

## [1.3.1]

- 修复 `ConditionSegment.next` 字段缺少 `@ToString.Exclude` / `@EqualsAndHashCode.Exclude`，导致链表调用 `toString()` / `equals()` / `hashCode()` 时递归穿透引发 `StackOverflowError`
- 修复 `SqlContext` 使用 `@EqualsAndHashCode(callSuper = true)` 继承上述递归行为，改为 `callSuper = false`
- 修复 `LambdaUtils.resolveGetterPropertyName` 对 `is` 前缀不检查后续字符大小写，导致 `isolation()` 等方法名被误截断为 `olation`
- 修复 `BoosterMapperUtils.getMapperContent` 未对 `GenericTypeUtils.resolveTypeArguments` 返回 `null` 做防护，触发 NPE
- 修复 `StrConvertUtils.camelCaseToUnderscore` 对连续大写缩写（如 `XMLParser`、`getHTTPURL`）插入多余下划线，现在正确输出 `xml_parser` / `get_http_url`
- 修复 `Condition.equals()` / `hashCode()` 依赖 `Object.equals` 对数组做引用比较，导致 `LinkedHashSet<Condition>` 无法对相同数组值的条件去重；覆写为深比较

## [1.3.0]

- 添加 LambdaSqlBuilder 条件开关，所有条件方法支持 `boolean condition` 前缀重载
- 修复 Condition.value 默认值从 `""` 改为 `null`，消除语义歧义
- 修复 Lambda getter 解析误接受 `getaway()` 等非 JavaBean getter，避免查询条件被静默忽略
- 修复 SqlContext 相等性忽略条件链的问题
- 修复 toSqlContext 副本共享集合、Map 和数组值的问题
- 修复 PageHelper 分页参数溢出及非正数未校验的问题
- 修复多 SqlSessionFactory 场景下自动配置因单实例注入失败的问题
- 修复 voByIds 空结果返回不可变集合的问题

## [1.2.0]

- 添加 TableMetaRegistry 统一表元数据注册中心和 TableResolver 接口
- 添加 BoosterAutoConfiguration Spring Boot 自动配置
- 添加 BoosterRegistry、BoosterEntity、BoosterService 接口
- 添加 PageHelper 扩展（PhMapper、PhPage）
- 添加自定义后缀规则支持（fromMap / fromBean）
- 添加 SqlBuilder.boost() 切换到指定 Booster 执行查询
- 重构包结构，model 类迁出到独立包
- 重构 SqlKeyword 枚举为 HashMap + 别名机制
- 废弃 lambdaBuilder()、build()、appendByMap()、bitAny() 等旧方法

## [1.1.0]

- 添加 LambdaBooster 链式查询构造器
- 添加 SqlBuilder 独立构造器
- 添加位运算操作符（hasAnyBits、hasAllBits、hasNoBits）
- 添加操作符后缀映射（Gte、Lte、Like、In、IsNull 等）
- 重构 Booster 接口为双泛型参数

## [1.0.0]

- 初始版本发布

---

[1.3.1]: https://github.com/luminion/sql-booster/compare/1.3.0...1.3.1
[1.3.0]: https://github.com/luminion/sql-booster/compare/1.2.0...1.3.0
[1.2.0]: https://github.com/luminion/sql-booster/compare/1.1.0...1.2.0
[1.1.0]: https://github.com/luminion/sql-booster/compare/1.0.0...1.1.0
[1.0.0]: https://github.com/luminion/sql-booster/releases/tag/1.0.0
