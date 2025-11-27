# SQL Booster

[![Maven Central](https://img.shields.io/maven-central/v/io.github.luminion/sql-booster)](https://mvnrepository.com/artifact/io.github.luminion/sql-booster)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![GitHub](https://img.shields.io/github/stars/luminion/sql-booster?style=social)](https://github.com/luminion/sql-booster)


SQL Booster 是一个数据库查询设计的增强工具包，旨在简化和增强数据访问层的开发。提供了强大的动态SQL动态条件和后缀查询映射功能。


## 功能特性

- **后缀动态映射SQL**：支持`字段`+`后缀`自动映射不同类型查询
- **条件动态拼装SQL**：支持根据入参动态拼接条件
- **Map查询条件**：自动转化Map参数
- **数据字段映射**：自动转换属性为数据库字段
- **SQL反注入**：通过预编译SQL, 防止SQL注入
- **Lambda链式调用**：支持链式调用追加参数条件
- **VO类型转化**：自动将查询结果转化为指定类
- **额外查询方法**：添加`voById`、`voByIds`、`voFirst`、`voUnique`、`voList`、`voPage`等方法

---

## Maven 依赖

[![Maven Central](https://img.shields.io/maven-central/v/io.github.luminion/sql-booster)](https://mvnrepository.com/artifact/io.github.luminion/sql-booster)

```xml

<dependency>
    <groupId>io.github.luminion</groupId>
    <artifactId>sql-booster</artifactId>
    <version>latest</version>
</dependency>
```

快照版本仓库地址
```xml
<repositories>
  <repository>
    <name>Central Portal Snapshots</name>
    <id>central-portal-snapshots</id>
    <url>https://central.sonatype.com/repository/maven-snapshots/</url>
    <releases>
      <enabled>false</enabled>
    </releases>
    <snapshots>
      <enabled>true</enabled>
    </snapshots>
  </repository>
</repositories>
```

---

## 快速开始

[code-generator](https://github.com/luminion/code-generator)代码生成器已适配该框架, 可以一键生成代码, 开箱即用

### 1. 创建实体类

若已有实体类, 可忽略该步骤

```java
// 数据库对应的实体类
public class SysUser {
    private Long id;
    private String name;
    private Integer age;
    // getter/setter...
}
```

```java
// 用于封装查询结果的VO类, 可以继承自实体类, 也可以直接使用实体类
public class SysUserVO {
    private Long id;
    private String name;
    private Integer age;
    // getter/setter...
}
```

### 2. mapper对应xml文件引用sql片段

获取xml文件内容

```java
import io.github.luminion.sqlbooster.util.MapperUtils;

public static void main(String[] args) {
    String mapperContent = MapperUtils.getMapperContent(SysUser.class, SysUserVO.class);
    System.out.println(mapperContent);
}
```

将获取的内容粘贴到mapper.xml文件中, 并根据需要`连表`/`添加条件`/`添加排序`

```xml
<!--复制工具类生成的该sql片段到mapper.xml文件中-->
<select id="selectByWrapper" resultType="com.example.test.vo.SysUserVO">
    SELECT
    a.*
    FROM
    sys_user a
    <where>
        <include refid="sqlbooster.conditions"/>
        <!--此处编写自定义条件SQL, 未自动映射的条件可通过param1.extra获取, 编写时以AND开头以兼容自动映射的查询条件-->
        AND a.deleted = 0
        <if test="param1.extra.userDeptName != null">
            AND a.dept_id in (SELECT id FROM sys_department WHERE name = #{param1.extra.userDeptName})
        </if>
    </where>
    <trim prefix="ORDER BY" prefixOverrides=",">
        <include refid="sqlbooster.sorts"/>
        <!--此处编写排序字段SQL, 编写时以,开头以兼容自动映射的排序-->
        , a.created_time DESC , a.id DESC
    </trim>
</select>
```

### 3. Mapper接口继承指定类

继承后可获得`voById`、`voByIds`、`voFirst`、`voUnique`、`voList`、`voPage`等方法

提供以下几种继承, 任选其一

* 继承`BoosterEngine`, 无分页功能
* 继承`PageHelperBooster`, 完整功能, 使用`PageHelper`分页(需自行引入`PageHelper`依赖)
* 继承`MybatisPlusBooster`, 完整功能, 使用`IPage`分页(需自行引入`Mybatis-plus`依赖),

#### Mybatis环境, 使用`PageHelperBooster`

```java
import io.github.luminion.sqlbooster.extension.pagehelper.BoosterPageHelperEngine;

// 继承PageHelperBooster
public interface SysUserMapper extends BoosterPageHelperEngine<SysUser, SysUserVO> {

}

```

> **下一步阅读: [使用示例](#使用示例)**

---

#### Mybatis-plus环境, 使用`MybatisPlusBooster`

针对Mybatis-plus环境, 提供了细分接口供`Service`/`ServiceImpl`/`Mapper`进行继承

- `BoosterMpMapper`继承了`BaseMapper`和`MybatisPlusBooster`
- `BoosterMpServiceImpl`继承`ServiceImpl`和`MybatisPlusBooster`
- `BoosterMpService`继承了`IService`和`MybatisPlusBooster`

```java
import io.github.luminion.sqlbooster.extension.mybatisplus.BoosterMpMapper;

// 继承BoosterBaseMapper
// eg: BoosterBaseMapper已继承BaseMapper, SysUserMapper无需继承原BaseMapper
public interface SysUserMapper extends BoosterMpMapper<SysUser, SysUserVO> {

}
```

```java
import io.github.luminion.sqlbooster.extension.mybatisplus.BoosterMpServiceImpl;

// 继承BoosterMpServiceImpl
// eg: BoosterMpServiceImpl已继承ServicImpl, SysUserServiceImpl无需继承原ServiceImpl
public class SysUserServiceImpl extends BoosterMpServiceImpl<SysUser, SysUserVO> {

}
```

```java
import io.github.luminion.sqlbooster.extension.mybatisplus.BoosterMpService;

// BoosterMpService
// eg: BoosterMpService已继承IService, SysUserService无需继承原IService
public class SysUserService extends BoosterMpService<SysUser, SysUserVO> {

}
```

> **下一步阅读: [使用示例](#使用示例)**

---

#### 自定义环境, 使用`BoosterEngine`, 不使用分页

* `BoosterEngine`提供了核心功能的多个默认实现, 但不包含分页功能
* `BoosterEngine`提供了`voById`、`voByIds`、`voFirst`、`voUnique`、`voList`等方法的实现
* `BoosterEngine`分页查询`voPage`方法在不调用时对业务逻辑无影响
* `BoosterEngine`分页查询`voPage`方法在调用时会抛出一个`UnsupportedOperationException`异常

```java
import io.github.luminion.sqlbooster.core.BoosterEngine;

// mapper继承BoosterEngine
public interface SysUserMapper extends BoosterEngine<SysUser, SysUserVO> {

}
```

> **下一步阅读: [使用示例](#使用示例)**

---

#### 自定义环境, 使用`BoosterEngine`, 并重写分页逻辑

* 默认该接口有4个不同参数的`voPage()`分页方法, 不使用分页功能时无需实现或重写
* 实际运行时会所有分页方法会最终重载到`voPage(Wrapper, long, long)`这个方法中
* 需要分页时, 仅重写`voPage(Wrapper, long, long)`方法, 添加分页的实现逻辑即可

建议抽象一个父接口书写逻辑, 继承`BoosterEngine`, 其他Mapper再继承该接口, 以免多次重写:

```java
import io.github.luminion.sqlbooster.core.BoosterEngine;

// 自定义全局接口, 继承BoosterEngine
public interface CustomBooster<T> extends BoosterEngine<SysUser, SysUserVO> {

    @Override
    default Page<SysUserVO> voPage(Wrapper<SysUser> queryParam, long pageNum, long pageSize) {
        // 查询预处理 - 提供给子类重写的方法, 可用于对wrapper进行预处理, 可以不调用, 但建议调用以规范行为
        voPreProcess(queryParam);

        // !!!重要!!!, 记得调用SqlHelper.process()方法
        // SqlBuilder.process()方法用于处理动态映射和后缀映射, 同时检查条件合法性, 防止sql注入
        BaseHelper<T> sqlBuilder = SqlHelper.of(queryParam).entity(this)
                .process(SuffixProcessor.of()::process);

        // 分页逻辑, 以下为Mybatis-plus的分页示例, 实际实现时替换为自己的即可
        PageDTO<V> pageInfo = new PageDTO<>(pageNum, pageSize);
        List<V> vs = selectByBooster(sqlBuilder, pageInfo); // 真正执行查询的mapper层方法
        pageInfo.setRecords(vs);
        MybatisPlusPage<V> page = new MybatisPlusPage<>(pageInfo);


        // 查询后处理 - 提供给子类重写的方法, 可用于对查询结果进行后处理, 可以不调用, 但建议调用以规范行为
        voPostProcess(page.getRecords(), sqlBuilder, page);
        return null;
    }

}
```

```java
// mapper继承自定义接口
public interface SysUserMapper extends CustomBooster<SysUser, SysUserVO> {


}
```

> **下一步阅读: [使用示例](#使用示例)**

<br/>

---

## 使用示例

```java
import io.github.luminion.sqlbooster.model.builder.SqlBuilder;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/user")
public class SysUserController {

    // 此处引入Mapper接口或Service接口
    @Autowired
    private SysUserMapper sysUserMapper;

    // 根据ID查询VO
    @GetMapping("/{id}")
    public SysUserVO getUserById(@PathVariable Long id) {
        return sysUserMapper.voById(id);
    }

    // 通过DTO对象查询
    @PostMapping("/dto")
    public List<SysUserVO> getUsersByDTO(@RequestBody SysUserDTO dto) {
        return sysUserMapper.voList(dto);
    }

    // 通过map条件查询(支持后缀映射不同类型查询)
    @PostMapping("/map")
    public List<SysUserVO> getUsersByMap(@RequestBody Map<String, Object> params) {
        return sysUserMapper.voList(params);
    }

    // 使用SqlHelper作为参数时, 前端可通过入参动态指定条件及排序
    @PostMapping("/sql")
    public List<SysUserVO> getUsersBySql(@RequestBody SqlBuilder<SysUser> sqlBuilder) {
        return sysUserMapper.voList(sqlBuilder);
    }

    // lambda调用,添加必要条件, 例如权限角色等
    @PostMapping("/lambda")
    public List<SysUserVO> getUsersBySql(@RequestBody Map<String, Object> params) {
        return SqlBuilder.of(SysUser.class)
                .append(params) // 合并或添加条件, 支持实体类, DTO对象, map, SqlHelper等
                .eq(SysUser::getState, 1) // state=1
                .ge(SysUser::getAge, 18) // age>=18
                .in(SysUser::getRoleId, Arrays.asList(1, 2))
                .like(SysUser::getUserName, "tom") // userName like '%tom%'
                .boost(sysUserMapper)
                .voList();
    }

    // 分页查询
    @PostMapping("/page/{current}/{size}")
    public IPage<SysUserVO> getUserPage(@RequestBody Map<String, Object> params,
                                        @PathVariable("current") Long current,
                                        @PathVariable("size") Long size) {
        return sysUserMapper.voPage(params, current, size);
    }

    // 2025年11月
    // 因EasyExcel停更, 续作FastExcel变更为Apache fesod
    // Apache fesod暂未孵化, 待api稳定后, 再提供Excel相关功能
//    // Excel导出
//    @PostMapping("/excel/export/{current}/{size}")
//    public void exportExcel(@RequestBody Map<String, Object> params,
//                            @PathVariable("current") Long current,
//                            @PathVariable("size") Long size) {
//        sysUserMapper.exportExcel(fileName, SysUserVO.class);
//    }
}
```

---

## 核心功能

### 动态后缀映射SQL

- 在`参数`名称后添加特殊的后缀, 可以`动态映射`为`不同类型`的查询
- 在不添加后缀时, 等同于`等于`查询
- 后端可用`实体类`或`Map`接收参数

#### 动态后缀映射表

| sql操作符号       | W操作说明      | 后缀                           | 示例 (JSON Key)                                           | 值类型                         |
|---------------|------------|------------------------------|---------------------------------------------------------|-----------------------------|
| `=`           | 等于         | (无)                          | `"name": "mike"`                                        | String, Number, Boolean     |
| `<>`          | 不等于        | `Ne` / `_ne`                 | `"ageNe": 18` / `"age_ne": 18`                          | String, Number, Boolean     |
| `<`           | 小于         | `Lt` / `_lt`                 | `"ageLt": 18` / `"age_lt": 18`                          | Number, Date                |
| `<=`          | 小于等于       | `Lte` / `_lte`               | `"ageLte": 18` / `"age_lte": 18`                        | Number, Date                |
| `>`           | 大于         | `Gt` / `_gt`                 | `"ageGt": 18` / `"age_gt": 18`                          | Number, Date                |
| `>=`          | 大于等于       | `Gte` / `_gte`               | `"ageGte": 18` / `"age_gte": 18`                        | Number, Date                |
| `LIKE`        | 模糊匹配       | `Like` / `_like`             | `"nameLike": "mike"` / `"name_like": "mike"`            | String                      |
| `NOT LIKE`    | 反模糊匹配      | `NotLike` / `_not_like`      | `"nameNotLike": "mike"` / `"name_not_like": "mike"`     | String                      |
| `IN`          | IN 查询      | `In` / `_in`                 | `"stateIn": [1, 2, 3]` / `"state_in": [1, 2, 3]`        | List/Array (String, Number) |
| `NOT IN`      | NOT IN 查询  | `NotIn` / `_not_in`          | `"stateNotIn": [1, 2, 3]` / `"state_not_in": [1, 2, 3]` | List/Array (String, Number) |
| `IS NULL`     | 为空         | `IsNull` / `_is_null`        | `"nameIsNull": true` / `"name_is_null": true`           | Boolean (true)              |
| `IS NOT NULL` | 不为空        | `IsNotNull` / `_is_not_null` | `"nameIsNotNull": true` / `"name_is_not_null": true`    | Boolean (true)              |
| `& ? > 0`     | 包含任意指定bit位 | `BitAny` / `_bit_any`        | `"permissionBitAny": 4` / `"permission_bit_any": 4`     | Number                      |
| `& ? = ?`     | 包含所有指定bit位 | `BitAll` / `_bit_all`        | `"permissionBitAll": 4` / `"permission_bit_all": 4`     | Number                      |
| `& ? = 0`     | 不包含指定bit位  | `BitNone` / `_bit_none`      | `"permissionBitNone": 4` / `"permission_bit_none": 4`   | Number                      |

#### 入参示例

查询`name`包含`mike`, `version`为`1`, `age`在`18-60`之间, `state`为`1`或`2`或`3`数据:

```json
{
  "nameLike": "mike",
  "version": 1,
  "ageGe": 18,
  "ageLt": 60,
  "stateIn": [1, 2, 3]
}
```

#### 自定义后缀映射

- 修改`SuffixProcessor`的默认后缀, 来改变默认的后缀映射
- 重写`BoosterEngine`验证调用的方法, 改变指定实例的后缀映射
- 创建`SqlHelper<T>`时, 调用`process()`处理方法, 处理单次映射
- 可用操作符见[动态后缀映射表](#动态后缀映射表)

自定义示例:

```java
import processor.io.github.luminion.sqlbooster.model.builder.SuffixProcessor;

@SpringBootApplication
public class App {

    public static void main(String[] args) throws Exception {
        SpringApplication.run(App.class, args);

        // 指定后缀和操作符的映射关系
        HashMap<String, String> map = new HashMap<String, String>();
        map.put("_like", "LIKE");
        map.put("_ge", ">=");
        map.put("_le", "<=");
        map.put();
        map.put("_like", "LIKE");
        map.put("_not_eq", "<>");
        // 设置默认后缀映射
        SuffixProcessor.defaultSuffixMap(map);
    }
}
```

### 动态条件组合SQL

- 入参自由指定`查询条件`/`查询类型`/`查询值`
- 入参自由指定`排序条件`和`升降序`
- 支持嵌套`子条件`和`or`条件
- 自动验证参数, 防止`SQL注入`
- 支持复杂条件自由组合

#### 入参格式
支持动态sql的入参类为[SqlHelper](src/main/java/io/github/luminion/sqlbooster/model/builder/SqlHelper.java)类,
其格式如下:

```json
{
  "conditions": [
    {
      "field": "name",
      "operator": "=",
      "value": "mike"
    },
    {
      "field": "age",
      "operator": ">=",
      "value": 18
    }
  ],
  "sorts": [
    {
      "field": "id",
      "asc": true
    },
    {
      "field": "age",
      "asc": false
    }
  ]
}
```

属性说明:

* `conditions`用于存放一组查询条件,
    * `field` 指定需要查询的字段
    * `operator` 指定查询类型, 不传递时默认为`=`
    * `value` 指定查询的值
* `sorts`用于存放一组排序条件
    * `field`指定需要排序的字段
    * `asc`指定是否升序, `true`为升序,`false`为降序, 不传递时默认为`false`

#### 查询类型映射表

| sql操作符号       | 说明         | operator参数值                  | 值类型                         |
|:--------------|:-----------|:-----------------------------|:----------------------------|
| `=`           | 等于         | 无需传参                         | Any                         |
| `<>`          | 不等于        | `<>`, `!=`, `ne`             | Any                         |
| `>`           | 大于         | `>`, `gt`                    | Number, Date                |
| `>=`          | 大于等于       | `>=`, `gte`                  | Number, Date                |
| `<`           | 小于         | `<`, `lt`                    | Number, Date                |
| `<=`          | 小于等于       | `<=`, `lte`                  | Number, Date                |
| `LIKE`        | 模糊匹配       | `like`                       | String                      |
| `NOT LIKE`    | 反模糊匹配      | `not_like`, `not like`       | String                      |
| `IN`          | 在指定列表中     | `in`                         | List/Array (String, Number) |
| `NOT IN`      | 不在指定列表中    | `not_in`, `not in`           | List/Array (String, Number) |
| `IS NULL`     | 为空         | `is_null`, `is null`         | Boolean (true)              |
| `IS NOT NULL` | 不为空        | `is_not_null`, `is not null` | Boolean (true)              |
| `& ? > 0`     | 包含任意指定bit位 | `bit_any`, `bit any`         | Number                      |
| `& ? = ?`     | 包含所有指定bit位 | `bit_all`, `bit all`         | Number                      |
| `& ? = 0`     | 不包含指定bit位  | `bit_none`, `bit none`       | Number                      |

> **提示: 其中 `=` 为默认值, 当查询的类型为`=`时无需传递operator参数**

#### 基本使用

查询`name`为`mike`, `version`大于等于`1`, `state`为`1`或`2`或`3`的数据

```json
{
  "conditions": [
    {
      "field": "name",
      "value": "mike"
    },
    {
      "field": "version",
      "operator": ">=",
      "value": 1
    },
    {
      "field": "state",
      "operator": "IN",
      "value": [1, 2, 3]
    }
  ]
}
```

#### 指定排序字段

查询`name`为`mike`, `version`为`1`的数据, 并将结果按照`id`降序, `age`升序排列

```json
{
  "conditions": [
    {
      "field": "name",
      "value": "mike"
    },
    {
      "field": "version",
      "value": 1
    }
  ],
  "sorts": [
    {
      "field": "id"
    },
    {
      "field": "age",
      "asc": true
    }
  ]
}
```

#### 完整功能 / 复杂查询

上文内容简化了SqlHelper结构, 实际结构为:

```json
{
  "connector": "and",
  "conditions": [
    {
      "field": "name",
      "operator": "=",
      "value": "mike"
    },
    {
      "field": "age",
      "operator": ">=",
      "value": 18
    }
  ],
  "sorts": [
    {
      "field": "id"
    },
    {
      "field": "age",
      "asc": true
    }
  ],
  "child": {
    "conditions": [],
    "connector": "or",
    "child": null
  }
}
```
字段属性说明:
* `connector`用于指定`conditions`中条件之间的关系
    * 该属性为可忽略属性 , 无需传递, 默认为`and`
    * 可选值: `and`并且 / `or`或者
* `conditions`用于存放一组查询条件,
    * `field` 指定需要查询的字段
    * `operator` 指定查询类型, 不传递时默认为`=`
    * `value` 指定查询的值
* `sorts`用于存放一组排序条件
    * `field`指定需要排序的字段
    * `asc`指定是否升序, `true`为升序,`false`为降序, 不传递时默认为`false`
* `child`为可嵌套的子条件, `仅复杂条件时`
    * 无需嵌套时可忽略该属性
    * 嵌套层级下不接受`sorts`参数
    * 必要时,嵌套层级下还可继续嵌套`child`


最佳实践:
- 需要同时满足的条件都放在根节点的`conditions`中
- `connector`默认为`AND`,不组合`OR`条件时无需传递
- `child`不使用时, 无需传递
- 当同时遇到即需要满足几个必要条件, 又存在多个条件任意满足其一时, 将这组条件放入一个`child`节点中,指定`connector`为`OR`

```json
{
  "conditions": [],
  "sorts": [],
  "child": {
    "conditions": [],
    "connector": "OR",
    "child": {
      "conditions": [],
      "connector": "OR"
    }
  }
}
```


复杂条件示例: 查询国籍为中国, 手机号不为空,姓名为mike或者john, 年龄小于18岁或者大于60岁的数据

```sql
SELECT *
FROM sys_user
WHERE (country = 'china' AND mobile IS NOT NULL)
  AND (name = 'mike' OR name = 'john')
  AND (age < 18 OR age > 60)
```

输入参数:

```json
{
  "conditions": [
    {
      "field": "country",
      "value": "china"
    },
    {
      "field": "mobile",
      "operator": "is_not_null",
      "value": true
    }
  ],
  "child": {
    "connector": "or",
    "conditions": [
      {
        "field": "name",
        "value": "mike"
      },
      {
        "field": "name",
        "value": "john"
      }
    ],
    "child": {
      "connector": "or",
      "conditions": [
        {
          "field": "age",
          "operator": "<",
          "value": 18
        },
        {
          "field": "age",
          "operator": ">",
          "value": 60
        }
      ]
    }
  }
}
```

## 包结构说明

```
io.github.luminion.sqlbooster
├── core                  # 核心的 SQL 构建器和引擎
│   ├── Page.java
│   ├── Booster.java
│   ├── BoosterCore.java
│   ├── BoosterEngine.java
│   └── MethodReference.java
├── util                  # 提供项目使用的各种工具类
│   ├── BoostUtils.java
│   ├── ExcelUtils.java
│   ├── MapperUtils.java
│   └── ReflectUtils.java
├── model                 # 定义了 API、SQL 和枚举等数据模型
│   ├── api
│   ├── sql
│   └── enums
├── config                # 提供 Spring Boot 的自动配置功能
│   └── BoosterAutoConfiguration.java
├── provider              # 包含各种提供者接口和实现，用于扩展和自定义
│   ├── support
│   ├── BoostProvider.java
│   ├── TableNameProvider.java
│   ├── IdPropertyProvider.java
│   ├── GetterPropertyProvider.java
│   └── PropertyToColumnAliasMapProvider.java
└── extension             # 提供了与 MyBatis、MyBatis-Plus 和 PageHelper 等第三方库的集成
    ├── mybatis
    ├── pagehelper
    └── mybatisplus
```