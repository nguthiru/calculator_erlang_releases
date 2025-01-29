# Erlang - All About Releases with OTP

This is a simple example demonstrating how to create and upgrade releases in Erlang. A lot of credit goes to the book [Learn You Some Erlang](https://learnyousomeerlang.com/).

I found it difficult to learn about releases since there was no practical, short, and quick example to demonstrate how it's done. So, I created one. 

This guide demonstrates how to create releases using OTP tools, `systools`, and `reltools`.

---

## Getting Started

Clone the repository and ensure Erlang is installed on your system.

---

## The Basics

### 1. Compiling Source Files

Erlang compiles `.erl` files into `.beam` files, so let's first compile them and move the output to an `ebin` directory.

```bash
mkdir -p ebin
# Compile Erlang source files
cd src
erlc *.erl

cd ..
# Move compiled files to the ebin directory
mv src/*.beam ebin/
# Copy the application file
cp src/calculator.app.src ebin/calculator.app
```

---

### 2. Building Releases

Create a `releases` folder in the project's root directory:

```bash
mkdir -p releases
```

This sets up the project for building releases.

---

## All About Releases

Releases in Erlang are powerful—they bundle everything needed to run your application, even if Erlang isn't installed on the target system. Additionally, they allow **hot upgrades**, reducing downtime by eliminating the need for canary deployments and rolling upgrades.

---

### Systools

`Systools` is one of the tools used to build Erlang releases. It's relatively easy to set up, requiring only an intuitive `.rel` file.

#### The `.rel` File

The `.rel` file describes your application's version, which should ideally match your `.app` file. It also lists dependencies that will be included in the release.

Example for `calculator-1.0.0.rel`:

```erlang
{release, {"calculator", "1.0.0"}, {erts, "15.2"},
 [{kernel, "10.2"},
  {stdlib, "6.2"},
  {sasl, "4.2.2"},
  {calculator, "1.0.0"}]}.
```

- **`calculator`**: The application name and version.
- **`erts`**: The Erlang Run-Time System version.
- **Included applications**:
  - `kernel`
  - `stdlib`
  - `sasl`
  - `calculator`

---

#### Creating a Release

Start the Erlang shell using `erl`.

##### Step 1: Create Boot and Start Scripts

Erlang requires **boot** and **start** scripts to initialize the release.

```erlang
1> systools:make_script("calculator-1.0.0", [{outdir, "releases"}, {path, ["ebin"]}]).
```

- **First argument**: The release name. Erlang looks for a corresponding `.rel` file.
- **`outdir`**: Specifies the output directory (`releases` folder).
- **`path`**: Directories containing compiled `.beam` and `.app` files.

##### Step 2: Create the Release Archive

Erlang packages releases into a compressed tarball (`.tar.gz`) for easy distribution.

```erlang
1> systools:make_tar("calculator-1.0.0", [{outdir, "releases"}, {path, ["ebin", "releases"]}, {erts, "/usr/local/lib/erlang"}]).
```

- **`outdir`**: The output directory.
- **`path`**: Search paths for compiled files (`ebin`) and upgrade scripts (`releases`).
- **`erts`**: Copies necessary Erlang runtime files to create a **self-contained** release.

This generates `calculator-1.0.0.tar.gz` in the `releases` directory.

---

### Installing the Release

Releases can be deployed on any machine or directory.

#### Step 1: Extract the Release

Copy the `.tar.gz` file to another directory and extract it:

```bash
tar -xvf calculator-1.0.0.tar.gz -C calculator_qa
```

#### Step 2: Run the Release

Navigate into the extracted folder and start the release:

```bash
cd calculator_qa
./erts-15.2/bin/erl -boot calculator-1.0.0/start
```

*(Replace `15.2` with the correct version of `erts` in your system.)*

### Upgrading the Release

I found this to be a slightly complex process, but we can make it through.

#### Step 1: Code Changes

Let's upgrade our `calculator` module by adding a power function.

Modify `calculator.erl`:

```erlang
-export([add/2, subtract/2, multiply/2, divide/2, pow/1]).

pow(A) -> A * A.
```

#### Step 2: Compiling and Updating the App Version

Adjust the app version number in `calculator.app.src` from `1.0.0` to `1.1.0`:

```erlang
{application, calculator,
 [{description, "A simple calculator application"},
  {vsn, "1.1.0"},
  {modules, [calculator, calculator_app, calculator_sup]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {calculator_app, []}}]}.
```

Then compile and update:

```bash
erlc src/*.erl
# Move compiled files to the ebin directory
mv src/*.beam ebin/
# Copy the application file
cp src/calculator.app.src ebin/calculator.app
```

---

#### Step 3: The Appup File

An **appup** file in Erlang is used for **hot code upgrades** and describes how an application should be upgraded or downgraded.
It consists of a list of instructions that tell the release handler how to handle changes to modules, applications, and configurations.

Create a new file `ebin/calculator.appup` with the following content:

```erlang
{"1.1.0",  % New version
 [{"1.0.0", [{load_module, calculator}]}],  % Upgrade instructions
 [{"1.0.0", [{load_module, calculator}]}]   % Downgrade instructions
}.
```

---

##### Possible Upgrade Instructions

1. **`load_module`** - Loads a new version of a module into memory. Used when code changes but does not affect the process state. Since our `pow/1` function does not affect the process state, we use `load_module`.
2. ```erlang
    {load_module, calculator}
   ```

3. **`update`** - Updates a module, replacing the existing one. Can include strategies like `soft_purge` (default) or `brutal_purge` to handle old processes.

   ```erlang
   {update, my_module, []}  % Soft purge
   {update, my_module, [brutal_purge]}  % Forcefully remove old versions
   ```

4. **`remove`** - Removes a module from the system. Used when a module is no longer needed.

   ```erlang
   {remove, old_module}
   ```

---

##### Additional Upgrade Instructions

For supervision and process handling:

- **`add_module`**: Adds a new module to the application.
- **`delete_module`**: Deletes a module that is no longer needed.
- **`restart_application`**: Restarts the entire application. Use this when significant changes affect the whole application.
- **`restart_new`**: Stops the old version of the module and starts a new one.

For application and supervisor management:

- **`remove_application`**: Completely removes an application from the node.
- **`add_application`**: Adds a new application dynamically.
- **`remove_supervisor`**: Removes a supervisor.
- **`add_supervisor`**: Adds a supervisor dynamically.
- **`restart_supervisor`**: Restarts a specific supervisor.

---

##### Appup File Format Example

```erlang
{"1.1.0",
 [
  {update, my_module, [soft_purge]},
  {add_module, new_feature_module},
  {remove_module, old_module},
  {restart_supervisor, my_supervisor}
 ],
 [
  {update, my_module, [soft_purge]},
  {delete_module, new_feature_module},
  {add_module, old_module},
  {restart_supervisor, my_supervisor}
 ]}.
```

- The first list represents **upgrade** instructions to version `1.1.0`.
- The second list represents **downgrade** instructions to revert to an older version.

---

##### Placing the Appup File

The **appup** file should be placed in the `ebin` directory and named after the application (`calculator.appup`).

### Step 3: The release file
Let us define a new release file version `1.1.0` and name it `calculator-1.1.0`:

```erlang
{release, {"calculator", "1.1.0"}, {erts, "15.2"},
 [{kernel, "10.2"},
  {stdlib, "6.2"},
  {sasl, "4.2.2"},
  {calculator, "1.1.0"}]}.
```
Notice: A new version number on the release and also the calculator app
Also, maintain your previous version rel file it will be very important in the next step

### Step 4: The relup File

A **relup** file is generated automatically from an **appup** file when creating a new release using `systools:make_relup/3`. It contains instructions for upgrading and downgrading a release dynamically while the system is running.

It is **essential** when upgrading a release but is **not required** for a fresh installation.

Luckily, `systools` can generate this file for us:

```erlang
systools:make_relup("calculator-1.1.0", ["calculator-1.0.0"], ["calculator-1.0.0"], [{outdir, "releases"}, {path, "ebin"}]).
```

#### **Parameters:**
1. **Current version** (`calculator-1.1.0`)
2. **Previous release** that can be upgraded from (in this case, we allow `1.0.0 -> 1.1.0`).
3. **Release that can be downgraded to** (in this case, we allow `1.1.0 -> 1.0.0`).
4. **`outdir`**: Specifies the output directory (`releases` folder).
5. **`path`**: Specifies where the `appup` file is located (we stored it in `ebin`).

A **relup** file will be created in the `releases` directory.

---

### Step 5: Creating the Release

Let us create the release using the previously discussed techniques:

```erlang
systools:make_script("calculator-1.1.0", [{outdir, "releases"}, {path, ["ebin"]}]).
systools:make_tar("calculator-1.1.0", [{outdir, "releases"}, {path, ["ebin", "releases"]}, {erts, "/usr/local/lib/erlang"}]).
```

This will generate a **tar.gz** file in the `releases` directory.

---

### Step 6: The Upgrade Process

Copy the following files to our test system directory (`calculator_qa`). You can place them anywhere, but they **must be together**:

1. `calculator-1.1.0.tar.gz`
2. `calculator-1.1.0.rel`

**⚠️ Note:** These files must be placed together—this is crucial!

---

### Upgrading Using the Release Handler

We will perform the upgrade from our target system's Erlang shell.

#### **Step 1: Unpack the Release**
From within the **`calculator_qa`** directory, start the **previous version** of the Erlang shell and run:

```erlang
release_handler:unpack_release("../calculator-1.1.0").
```

*(Use the absolute path if possible.)*

#### **Step 2: Install the Release**
```erlang
release_handler:install_release("calculator-1.1.0").
```
This will add the new release to the `releases` folder in the target system directory.

#### **Step 3: Make the Release Permanent**
```erlang
release_handler:make_permanent("calculator-1.1.0").
```
Do not add the file extensions
---

That's it! This process is enough to **install and upgrade** a release in an Erlang system.
