# SQL Booster

[![Maven Central](https://img.shields.io/maven-central/v/io.github.luminion/sql-booster)](https://mvnrepository.com/artifact/io.github.luminion/sql-booster)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![GitHub stars](https://img.shields.io/github/stars/luminion/sql-booster?style=social)](https://github.com/luminion/sql-booster)

SQL Booster 是面向 MyBatis / MyBatis-Plus 的查询增强工具包。
框架以 `SqlContext<T>` 作为统一查询模型，提供 `voById`、`voList`、`voPage` 和 `lambdaBooster()` 等查询入口，用于组织动态 SQL 条件、排序和分页查询。

> 历史项目：[mybatis-plus-enhancer](https://github.com/bootystar/mybatis-plus-enhancer)

## 功能特性

- 统一的 `SqlContext<T>` 查询模型，适配动态 SQL 条件拼装场景
- 提供 `voById`、`voList`、`voPage` 三个核心查询入口
- 提供 `voByIds`、`voFirst`、`voUnique` 派生查询方法
- 支持 `lambdaBooster()` 链式构造条件、排序和分页参数
- 支持 Map / Bean 到 `SqlContext<T>` 的统一转换
- 支持原生 MyBatis、MyBatis-Plus、PageHelper 三类接入方式
- Mapper XML 可直接复用 `sqlbooster.conditions`、`sqlbooster.sorts` 片段

---

## Maven 依赖

```xml
<dependency>
    <groupId>io.github.luminion</groupId>
    <artifactId>sql-booster</artifactId>
    <version>1.1.0</version>
</dependency>
```

当前仓库源码如未发布，请按实际分支版本调整依赖版本。

---

## 快速开始

### 1. Mapper 继承接口

MyBatis-Plus：

```java
import io.github.luminion.sqlbooster.extension.mybatisplus.MpMapper;

public interface SysUserMapper extends MpMapper<SysUser, SysUserVO> {
}
```

原生 MyBatis：

```java
import io.github.luminion.sqlbooster.extension.mybatis.BoosterMapper;

public interface SysUserMapper extends BoosterMapper<SysUser, SysUserVO> {
}
```

### 2. XML 引入 `sqlbooster` 片段

`BoosterMapper` 通过 `selectByXml(SqlContext<T> sqlContext, Object page)` 执行实际查询。
`SqlContext<T>` 为统一查询入参，XML 可直接复用内置动态片段：

```xml
<mapper namespace="com.example.mapper.SysUserMapper">

    <select id="selectByXml" resultType="com.example.vo.SysUserVO">
        SELECT a.*
        FROM sys_user a
        <where>
            <include refid="sqlbooster.conditions"/>
        </where>
        <trim prefix="ORDER BY" prefixOverrides=",">
            <include refid="sqlbooster.sorts"/>
            , a.id DESC
        </trim>
    </select>

</mapper>
```

### 3. 调用核心方法

列表查询和分页查询统一通过 `SqlContext<T>` 传入动态条件。

```java
@RestController
@RequestMapping("/user")
public class SysUserController {

    @Resource
    private SysUserMapper sysUserMapper;

    @GetMapping("/{id}")
    public SysUserVO detail(@PathVariable Long id) {
        return sysUserMapper.voById(id);
    }

    @PostMapping("/list")
    public List<SysUserVO> list(@RequestBody SysUserQuery query) {
        SqlContext<SysUser> sqlContext = SqlBuilder.of(SysUser.class)
                .fromBean(query)
                .toSqlContext();
        return sysUserMapper.voList(sqlContext);
    }

    @PostMapping("/page/{pageNum}/{pageSize}")
    public BPage<SysUserVO> page(@RequestBody SysUserQuery query,
                                 @PathVariable Long pageNum,
                                 @PathVariable Long pageSize) {
        SqlContext<SysUser> sqlContext = SqlBuilder.of(SysUser.class)
                .fromBean(query)
                .toSqlContext();
        return sysUserMapper.voPage(sqlContext, pageNum, pageSize);
    }
}
```

### 4. 实体实现默认 Booster 入口

实体可实现 `BoosterEntity<T, V>` 获取默认查询入口：

```java
import io.github.luminion.sqlbooster.core.BoosterEntity;

public class SysUser implements BoosterEntity<SysUser, SysUserVO> {
}
```

```java
SysUser user = new SysUser();

SysUserVO detail = user.booster().voById(1L);

List<SysUserVO> list = user.lambdaBooster()
        .eq(SysUser::getState, 1)
        .list();
```

---

## 可用扩展接口

| 接口 / 类型 | 说明 | 包路径 |
|---|---|---|
| `Booster` | 核心查询契约 | `io.github.luminion.sqlbooster.core` |
| `BoosterEntity` | 实体侧默认查询入口 | `io.github.luminion.sqlbooster.core` |
| `BoosterService` | Service 侧默认查询入口 | `io.github.luminion.sqlbooster.core` |
| `BoosterMapper` | 原生 MyBatis Mapper 扩展，不含分页实现 | `io.github.luminion.sqlbooster.extension.mybatis` |
| `MpMapper` | MyBatis-Plus Mapper 扩展，内置分页实现 | `io.github.luminion.sqlbooster.extension.mybatisplus` |
| `MpService` | MyBatis-Plus Service 扩展接口 | `io.github.luminion.sqlbooster.extension.mybatisplus` |
| `MpServiceImpl` | MyBatis-Plus Service 扩展基类 | `io.github.luminion.sqlbooster.extension.mybatisplus` |
| `PhMapper` | PageHelper Mapper 扩展，内置分页实现 | `io.github.luminion.sqlbooster.extension.pagehelper` |
| `SqlBuilder` | `SqlContext<T>` 构造器 | `io.github.luminion.sqlbooster.builder` |
| `LambdaBooster` | Lambda 链式查询器 | `io.github.luminion.sqlbooster.core` |
| `BoosterRegistry` | 默认 Booster 注册表 | `io.github.luminion.sqlbooster.metadata` |

---

## 使用示例

### 1. Mapper / Builder / Lambda

```java
import io.github.luminion.sqlbooster.builder.SqlBuilder;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.model.SqlContext;

SqlContext<SysUser> sqlContext = SqlBuilder.of(SysUser.class)
        .fromMap(params)
        .eq(SysUser::getState, 1)
        .orderByDesc(SysUser::getId)
        .toSqlContext();

SysUserVO detail = sysUserMapper.voById(1L);
List<SysUserVO> list = sysUserMapper.voList(sqlContext);
BPage<SysUserVO> page = sysUserMapper.voPage(sqlContext, 1L, 20L);

List<SysUserVO> lambdaList = sysUserMapper.lambdaBooster()
        .eq(SysUser::getState, 1)
        .like(SysUser::getUserName, "tom")
        .list();
```

### 2. 实体直连 Booster

```java
import io.github.luminion.sqlbooster.core.BoosterEntity;

public class SysUser implements BoosterEntity<SysUser, SysUserVO> {
}

List<SysUserVO> list = new SysUser().lambdaBooster()
        .eq(SysUser::getState, 1)
        .list();
```

### 3. 使用 `BoosterRegistry`

`BoosterRegistry` 提供默认 Booster 的注册与获取能力。

```java
import io.github.luminion.sqlbooster.core.Booster;
import io.github.luminion.sqlbooster.metadata.BoosterRegistry;

Booster<SysUser, SysUserVO> booster = BoosterRegistry.getRequiredBooster(SysUser.class, SysUserVO.class);

List<SysUserVO> list = booster.lambdaBooster()
        .eq(SysUser::getState, 1)
        .list();
```

### 4. 自定义 Mapper 直接接收 `SqlContext`

`SqlBuilder.toSqlContext()` 的结果可直接作为自定义 Mapper 入参：

```java
public interface SysUserCustomMapper {

    List<SysUserLiteVO> selectLite(SqlContext<SysUser> sqlContext);
}
```

```java
SqlContext<SysUser> sqlContext = SqlBuilder.of(SysUser.class)
        .fromMap(params)
        .eq(SysUser::getState, 1)
        .orderByDesc(SysUser::getId)
        .toSqlContext();

List<SysUserLiteVO> list = sysUserCustomMapper.selectLite(sqlContext);
```

对应 XML：

```xml
<select id="selectLite" resultType="com.example.vo.SysUserLiteVO">
    SELECT a.id, a.user_name
    FROM sys_user a
    <where>
        <include refid="sqlbooster.conditions"/>
    </where>
    <trim prefix="ORDER BY" prefixOverrides=",">
        <include refid="sqlbooster.sorts"/>
    </trim>
</select>
```

---

## 核心功能说明

### 1. `SqlContext<T>`

`SqlContext<T>` 是统一查询模型，用于承载：

- 查询条件
- 排序规则
- 动态参数
- 供 Mapper XML 直接消费的上下文数据

核心查询方法均围绕 `SqlContext<T>` 工作：

- `voById(id)`：主键查询
- `voList(sqlContext)`：列表查询
- `voPage(sqlContext, pageNum, pageSize)`：分页查询
- `selectByXml(sqlContext, page)`：Mapper XML 查询入口

适用场景包括前端条件透传、服务端动态条件拼装和 XML 统一消费。

### 2. 动态后缀映射

`fromMap` / `fromBean` 支持按字段后缀映射 SQL 操作符。
无后缀时默认按等值条件处理。

| 操作说明 | 后缀 | 示例 | 值类型 |
|---|---|---|---|
| 等于 | 无 | `"name": "mike"` | String, Number, Boolean |
| 不等于 | `Ne` | `"ageNe": 18` | String, Number, Boolean |
| 小于 | `Lt` | `"ageLt": 18` | Number, Date |
| 小于等于 | `Lte` | `"ageLte": 18` | Number, Date |
| 大于 | `Gt` | `"ageGt": 18` | Number, Date |
| 大于等于 | `Gte` | `"ageGte": 18` | Number, Date |
| 模糊匹配 | `Like` | `"nameLike": "mike"` | String |
| 反模糊匹配 | `NotLike` | `"nameNotLike": "mike"` | String |
| IN | `In` | `"stateIn": [1, 2, 3]` | List / Array |
| NOT IN | `NotIn` | `"stateNotIn": [1, 2, 3]` | List / Array |
| 为空 | `IsNull` | `"nameIsNull": true` | Boolean |
| 不为空 | `IsNotNull` | `"nameIsNotNull": true` | Boolean |
| 包含任意位 | `HasAnyBits` | `"roleHasAnyBits": 4` | Number |
| 包含全部位 | `HasAllBits` | `"roleHasAllBits": 4` | Number |
| 不包含位 | `HasNoBits` | `"roleHasNoBits": 4` | Number |

入参示例：

```json
{
  "nameLike": "mike",
  "version": 1,
  "ageGte": 18,
  "ageLt": 60,
  "stateIn": [1, 2, 3]
}
```

### 3. `SqlBuilder`

`SqlBuilder` 用于构造标准化 `SqlContext<T>`，常用入口包括：

- `fromMap(map)`
- `fromBean(bean)`
- `eq / ne / gt / gte / lt / lte`
- `like / notLike`
- `in / notIn`
- `isNull / isNotNull`
- `orderByAsc / orderByDesc`
- `toSqlContext()`

示例：

```java
SqlContext<SysUser> sqlContext = SqlBuilder.of(SysUser.class)
        .fromMap(params)
        .eq(SysUser::getState, 1)
        .gte(SysUser::getAge, 18)
        .orderByDesc(SysUser::getId)
        .toSqlContext();
```

### 4. `SqlContext` 结构

基础结构：

```json
{
  "conditions": [
    { "field": "name", "value": "mike" },
    { "field": "version", "operator": ">=", "value": 1 }
  ],
  "sorts": [
    { "field": "id" },
    { "field": "age", "asc": true }
  ]
}
```

多层条件嵌套：

```json
{
  "conditions": [
    { "field": "country", "value": "china" },
    { "field": "mobile", "operator": "is_not_null", "value": true }
  ],
  "next": {
    "and": false,
    "conditions": [
      { "field": "name", "value": "mike" },
      { "field": "name", "value": "john" }
    ],
    "next": {
      "and": false,
      "conditions": [
        { "field": "age", "operator": "<", "value": 18 },
        { "field": "age", "operator": ">", "value": 60 }
      ]
    }
  }
}
```

---

## 兼容说明

- 当前推荐入口为 `lambdaBooster()`，`lambdaBuilder()` 与 `sqlBuilder()` 仅保留为兼容方法
- `SqlBuilder` 推荐使用 `toSqlContext()`，`build()` 为兼容保留方法
- 查询方法统一使用 `voById`、`voList`、`voPage`

---

## 技术实现

### 1. `SqlBuilder` 到 `SqlContext` 的处理链

完整链路可以分成 4 段：

1. 构造阶段

- `SqlBuilder.of(Entity.class)` 或 `booster.lambdaBooster()` 作为入口。
- `eq / gte / like / in / orderBy...` 先生成最原始的 `Condition` / `Sort`。
- `or(...)` 不会把条件平铺到当前节点，而是生成新的 `ConditionSegment` 节点，保留分组语义。

2. Builder 归一化阶段

- `fromMap / fromBean` 会先把输入统一包装成“字段 + EQ + 值”的条件。
- 最终统一交给 `SqlContextUtils.buildWithSuffix(...)` 做标准化处理。
- `toSqlContext()` 返回的是副本，不暴露 builder 内部的可变对象。

3. `SqlContextUtils` 标准化阶段

- 通过 `TableMetaRegistry` 获取实体的“属性名 -> 列别名”映射。
- 先处理 `params` 中的后缀字段，再处理 `conditions` 中的字段。
- 对每个字段依次做：
  - 精确字段映射
  - 后缀拆解，例如 `ageGe -> age + Ge`
  - 操作符和值合法性校验
- 排序字段也会走同样的字段映射，只允许映射后的列名进入最终 SQL。

4. `Booster` 执行阶段

- `SqlBuilder#boost(booster)` 会把当前 builder 的 `SqlContext` 交给新的 `LambdaBooster`。
- `LambdaBooster#list/first/page` 最终回调到 `Booster#voList/voFirst/voPage`。
- `BoosterMapper#doFetch` 在真正执行前会再次调用 `normalize(...)`，保证外部手工传入的 `SqlContext` 也会被规范化。
- 最后由 Mapper XML 里的 `sqlbooster.conditions` 和 `sqlbooster.sorts` 片段生成 SQL。

可以把整体流程理解成：

`SqlBuilder / LambdaBooster`
-> 收集原始条件
-> `SqlContextUtils`
-> 字段映射、后缀解析、值校验
-> `Booster`
-> 转发给执行器
-> MyBatis XML
-> 渲染 SQL

### 2. `Booster` 在链路中的职责

`Booster` 本身不是 SQL 生成器，而是统一查询协议。

- `Booster#lambdaBooster()`：返回已绑定执行器的链式入口。
- `BoosterSupport`：提供 `voById / voList / voPage` 等默认实现，并把查询落到 `doFetch(...)`。
- `BoosterMapper`：MyBatis 场景下把标准化后的 `SqlContext` 交给 `selectByXml(...)`。
- `BoosterService`：自己不执行 SQL，只是从 `BoosterRegistry` 里找到默认 Booster 再转发调用。
- `BoosterRegistry`：按 `(entityClass, resultClass)` 维度管理默认 Booster。

所以：

- `SqlBuilder` 负责组条件
- `SqlContextUtils` 负责标准化
- `Booster` 负责把标准化后的上下文交给真正执行层

### 3. 怎么防 SQL 注入

防注入主要靠 3 层：

1. 值参数化

- 条件值统一通过 `#{...}` 绑定。
- `IN` 查询中的每个元素也逐个走 `#{val}`。
- 因此用户输入的值不会直接拼接到 SQL 字符串里。

2. 字段白名单映射

- XML 里虽然使用了 `${item.field}`，但 `item.field` 不是直接信任外部输入。
- 所有字段都必须先经过 `TableMetaRegistry` 的元数据映射。
- 只有这两类字段能进入最终 SQL：
  - 命中实体属性映射的字段
  - 已经等于合法列别名的字段
- 未识别字段只会进入 `params`，不会被内置 SQL 片段消费。

3. 操作符白名单

- 操作符必须先经过 `SqlKeyword.resolve(...)` 校验。
- 非法操作符不会进入最终 SQL。

边界说明：

- 如果你在自定义 XML 里自己对 `params` 使用 `${}` 拼接，那部分安全性由自定义 SQL 自己负责。
- 内置 `sqlbooster.conditions` / `sqlbooster.sorts` 的安全前提是：字段和操作符都先经过库内映射和校验。

### 4. 后缀是怎么映射的

`fromMap` / `fromBean` 的后缀处理发生在 `SqlContextUtils.buildWithSuffix(...)`。

默认后缀包括：

- `Ne`
- `Lt` / `Lte` / `Le`
- `Gt` / `Gte` / `Ge`
- `Like` / `NotLike`
- `In` / `NotIn`
- `IsNull` / `IsNotNull`
- `HasAnyBits` / `HasAllBits` / `HasNoBits`

同时支持下划线风格，例如：

- `_gte`
- `_not_like`
- `_is_null`

匹配规则如下：

1. 先按后缀长度倒序匹配

- 避免 `Like` 抢先匹配掉 `NotLike` 这类更长后缀。

2. 先尝试精确字段映射，再尝试“字段 + 后缀”拆解

- 例如 `ageGe -> age + Ge`
- 如果 `nameLike` 本身就是实体字段，那么会优先命中真实字段。
- 如果写成 `nameLikeLike`，则会拆成 `nameLike + Like`。

3. 命中后会转成标准 `Condition`

- `ageGe = 18` -> `a.age >= 18`
- `nameLike = "tom"` -> `a.name LIKE "%tom%"`
- `stateIn = [1,2,3]` -> `a.state IN (...)`

4. 自定义后缀有两种模式

- `buildWithSuffix(entityClass, source, customSuffixMap)`
  - 只使用自定义后缀，替换默认规则
- `buildWithSuffix(entityClass, source, customSuffixMap, true)`
  - 默认规则先铺底，再叠加自定义后缀
  - 同名后缀由自定义规则覆盖默认规则

### 5. 值校验规则

后缀或字段映射成功后，还会做一层值校验：

- `LIKE`：如果值里不含 `%`，会自动补成 `%value%`
- `IN / NOT IN`：空集合或空数组视为无效条件
- 比较运算：要求值实现 `Comparable`
- 位运算：只接受整数类型
- `IS NULL / IS NOT NULL`：只有值为 `true` 才生成条件

如果使用 strict 模式追加条件：

- 输入多少条件和排序，就要求成功解析多少
- 只要存在未识别字段或非法值，就直接抛异常
- 不再静默忽略
